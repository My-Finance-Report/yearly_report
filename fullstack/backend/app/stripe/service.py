import os
from datetime import datetime, timezone
from typing import Any, cast

import stripe
from sqlalchemy.orm import Session
from app.models.stripe import (
    Price,
    PriceId,
    Subscription,
    SubscriptionStatus,
    SubscriptionTier,
)
from app.models.transaction_source import TransactionSource
from app.models.user import User


from app.schemas.subscription import (
    CheckoutSession,
    PriceDetails,
    SubscriptionDetails,
    SubscriptionLimits,
)

# Initialize Stripe with API key
stripe.api_key = os.getenv("STRIPE_SECRET_KEY", "")
STRIPE_WEBHOOK_SECRET = os.getenv("STRIPE_WEBHOOK_SECRET", "")


class StripeService:
    @staticmethod
    def get_transaction_source_count(db: Session, user_id: int) -> int:
        """Get the count of active transaction sources for a user."""
        return (
            db.query(TransactionSource)
            .filter(TransactionSource.user_id == user_id, ~TransactionSource.archived)
            .count()
        )

    @staticmethod
    def check_subscription_limits(db: Session, user_id: int) -> SubscriptionLimits:
        """
        Check if user has reached their subscription limits.
        Returns subscription limits information
        """
        # Get user's subscription
        subscription = (
            db.query(Subscription, Price)
            .filter(Subscription.user_id == user_id)
            .join(Price, Subscription.price_id == Price.id)
            .first()
        )

        # Count active transaction sources
        source_count = StripeService.get_transaction_source_count(db, user_id)

        # Free tier allows 1 transaction source
        if not subscription or not subscription.price:
            has_reached_limit = source_count >= 1
            return SubscriptionLimits(
                has_reached_limit=has_reached_limit,
                current_count=source_count,
                max_allowed=1,
            )

        # Get max sources from price
        max_sources = subscription.price.max_sources
        has_reached_limit = source_count >= max_sources

        return SubscriptionLimits(
            has_reached_limit=has_reached_limit,
            current_count=source_count,
            max_allowed=max_sources,
        )

    @staticmethod
    def create_customer(db: Session, user: User) -> str:
        """Create a Stripe customer for a user."""
        if not user.email:
            raise ValueError("User must have an email to create a Stripe customer")

        # Check if user already has a customer ID
        subscription = (
            db.query(Subscription).filter(Subscription.user_id == user.id).first()
        )

        if subscription and subscription.stripe_customer_id:
            return subscription.stripe_customer_id

        # Create customer in Stripe
        customer = stripe.Customer.create(
            email=user.email,
            name=user.full_name or user.email,
            metadata={"user_id": str(user.id)},
        )

        # Create or update subscription record
        if not subscription:
            subscription = Subscription(
                user_id=user.id,
                stripe_customer_id=customer.id,
                status=SubscriptionStatus.active,
            )
            db.add(subscription)
        else:
            subscription.stripe_customer_id = customer.id

        db.commit()
        return customer.id

    @staticmethod
    def create_subscription(db: Session, user_id: int, price_id: int) -> Subscription:
        """Create a subscription for a user."""
        # Get user and price
        user = db.query(User).filter(User.id == user_id).first()
        if not user:
            raise ValueError("User not found")

        price = db.query(Price).filter(Price.id == price_id).first()
        if not price:
            raise ValueError("Price not found")

        # Ensure user has a Stripe customer
        customer_id = StripeService.create_customer(db, user)

        # Create subscription in Stripe
        stripe_subscription: stripe.Subscription = stripe.Subscription.create(
            customer=customer_id,
            items=[{"price": price.stripe_price_id}],
            expand=["latest_invoice.payment_intent"],
        )

        # Update or create subscription in database
        subscription = (
            db.query(Subscription).filter(Subscription.user_id == user_id).first()
        )

        if not subscription:
            subscription = Subscription(
                user_id=user_id,
                stripe_customer_id=customer_id,
                stripe_subscription_id=stripe_subscription.id,
                price_id=cast(PriceId, price_id),
                status=SubscriptionStatus(stripe_subscription.status),
                current_period_start=datetime.fromtimestamp(
                    stripe_subscription["current_period_start"], tz=timezone.utc
                ),
                current_period_end=datetime.fromtimestamp(
                    stripe_subscription["current_period_end"], tz=timezone.utc
                ),
                cancel_at_period_end=stripe_subscription.cancel_at_period_end,
            )
            db.add(subscription)
        else:
            subscription.stripe_subscription_id = stripe_subscription.id
            subscription.price_id = cast(PriceId, price_id)
            subscription.status = SubscriptionStatus(stripe_subscription.status)
            subscription.current_period_start = datetime.fromtimestamp(
                stripe_subscription["current_period_start"], tz=timezone.utc
            )
            subscription.current_period_end = datetime.fromtimestamp(
                stripe_subscription["current_period_end"], tz=timezone.utc
            )
            subscription.cancel_at_period_end = stripe_subscription.cancel_at_period_end

        db.commit()
        db.refresh(subscription)
        return subscription

    @staticmethod
    def cancel_subscription(db: Session, user_id: int) -> Subscription:
        """Cancel a user's subscription at the end of the billing period."""
        subscription = (
            db.query(Subscription).filter(Subscription.user_id == user_id).first()
        )

        if not subscription or not subscription.stripe_subscription_id:
            raise ValueError("No active subscription found")

        # Cancel subscription in Stripe
        stripe.Subscription.modify(
            subscription.stripe_subscription_id, cancel_at_period_end=True
        )

        # Update subscription in database
        subscription.cancel_at_period_end = True
        db.commit()
        db.refresh(subscription)
        return subscription

    @staticmethod
    def create_checkout_session(
        db: Session, user_id: int, price_id: int, success_url: str, cancel_url: str
    ) -> CheckoutSession:
        """Create a checkout session for a subscription."""
        # Get user and price
        user = db.query(User).filter(User.id == user_id).first()
        if not user:
            raise ValueError("User not found")

        price = db.query(Price).filter(Price.id == price_id).first()
        if not price:
            raise ValueError("Price not found")

        # Ensure user has a Stripe customer
        customer_id = StripeService.create_customer(db, user)

        # Create checkout session
        checkout_session = stripe.checkout.Session.create(
            customer=customer_id,
            payment_method_types=["card"],
            line_items=[
                {
                    "price": price.stripe_price_id,
                    "quantity": 1,
                }
            ],
            mode="subscription",
            success_url=success_url,
            cancel_url=cancel_url,
            metadata={"user_id": str(user_id), "price_id": str(price_id)},
        )

        if not checkout_session.url:
            raise ValueError("Failed to create checkout session")

        return CheckoutSession(checkout_url=checkout_session.url)

    @staticmethod
    def handle_webhook_event(
        db: Session, payload: bytes, sig_header: str
    ) -> dict[str, Any]:
        """Handle Stripe webhook events."""
        try:
            event = stripe.Webhook.construct_event(
                payload, sig_header, STRIPE_WEBHOOK_SECRET
            )  # type: ignore
        except ValueError as e:
            # Invalid payload
            raise ValueError(f"Invalid payload: {str(e)}")
        except stripe.SignatureVerificationError as e:
            # Invalid signature
            raise ValueError(f"Invalid signature: {str(e)}")

        # Handle the event
        if event.type == "checkout.session.completed":
            session = event.data.object
            StripeService._handle_checkout_completed(db, session)
        elif event.type == "customer.subscription.updated":
            subscription = event.data.object
            StripeService._handle_subscription_updated(db, subscription)
        elif event.type == "customer.subscription.deleted":
            subscription = event.data.object
            StripeService._handle_subscription_deleted(db, subscription)

        return {"status": "success"}

    @staticmethod
    def _handle_checkout_completed(db: Session, session: dict[str, Any]) -> None:
        """Handle checkout.session.completed event."""
        # Extract metadata
        user_id = int(session.get("metadata", {}).get("user_id", 0))
        price_id = int(session.get("metadata", {}).get("price_id", 0))

        if not user_id or not price_id:
            return

        # Get subscription ID
        subscription_id = session.get("subscription")
        if not subscription_id:
            return

        # Get subscription details from Stripe
        stripe_subscription = stripe.Subscription.retrieve(subscription_id)

        # Update subscription in database
        subscription = (
            db.query(Subscription).filter(Subscription.user_id == user_id).first()
        )

        if not subscription:
            # Create new subscription
            subscription = Subscription(
                user_id=user_id,
                stripe_subscription_id=subscription_id,
                price_id=cast(PriceId, price_id),
                stripe_customer_id=session.get("customer"),
                status=SubscriptionStatus(stripe_subscription.status),
                current_period_start=datetime.fromtimestamp(
                    stripe_subscription["current_period_start"], tz=timezone.utc
                ),
                current_period_end=datetime.fromtimestamp(
                    stripe_subscription["current_period_end"], tz=timezone.utc
                ),
                cancel_at_period_end=stripe_subscription.cancel_at_period_end,
            )
            db.add(subscription)
        else:
            # Update existing subscription
            subscription.stripe_subscription_id = subscription_id
            subscription.price_id = cast(PriceId, price_id)
            subscription.status = SubscriptionStatus(stripe_subscription.status)
            subscription.current_period_start = datetime.fromtimestamp(
                stripe_subscription["current_period_start"], tz=timezone.utc
            )
            subscription.current_period_end = datetime.fromtimestamp(
                stripe_subscription["current_period_end"], tz=timezone.utc
            )
            subscription.cancel_at_period_end = stripe_subscription.cancel_at_period_end

        db.commit()

    @staticmethod
    def _handle_subscription_updated(
        db: Session, subscription_data: dict[str, Any]
    ) -> None:
        """Handle customer.subscription.updated event."""
        subscription_id = subscription_data.get("id")
        if not subscription_id:
            return

        # Find subscription in database
        subscription = (
            db.query(Subscription)
            .filter(Subscription.stripe_subscription_id == subscription_id)
            .first()
        )

        if not subscription:
            return

        # Update subscription status
        subscription.status = SubscriptionStatus(
            subscription_data.get("status", "active")
        )
        subscription.current_period_start = datetime.fromtimestamp(
            subscription_data.get("current_period_start", 0), tz=timezone.utc
        )
        subscription.current_period_end = datetime.fromtimestamp(
            subscription_data.get("current_period_end", 0), tz=timezone.utc
        )
        subscription.cancel_at_period_end = subscription_data.get(
            "cancel_at_period_end", False
        )

        db.commit()

    @staticmethod
    def _handle_subscription_deleted(
        db: Session, subscription_data: dict[str, Any]
    ) -> None:
        """Handle customer.subscription.deleted event."""
        subscription_id = subscription_data.get("id")
        if not subscription_id:
            return

        # Find subscription in database
        subscription = (
            db.query(Subscription)
            .filter(Subscription.stripe_subscription_id == subscription_id)
            .first()
        )

        if not subscription:
            return

        # Update subscription status
        subscription.status = SubscriptionStatus.canceled
        subscription.cancel_at_period_end = False

        db.commit()

    @staticmethod
    def get_subscription_details(db: Session, user_id: int) -> SubscriptionDetails:
        """Get subscription details for a user."""
        subscription = (
            db.query(Subscription, Price)
            .join(Price, Price.id == Subscription.price_id)
            .filter(Subscription.user_id == user_id)
            .first()
        )

        if not subscription:
            return SubscriptionDetails(
                tier=SubscriptionTier.free,
                status=SubscriptionStatus.active,
                current_period_end=None,
                cancel_at_period_end=False,
                max_sources=1,
                current_sources=StripeService.get_transaction_source_count(db, user_id),
            )

        # Get price details
        price = subscription.price
        max_sources = 1  # Default for free tier

        if price:
            max_sources = price.max_sources

        return SubscriptionDetails(
            tier=price.tier if price else SubscriptionTier.free,
            status=subscription.status,
            current_period_end=subscription.current_period_end,
            cancel_at_period_end=subscription.cancel_at_period_end,
            max_sources=max_sources,
            current_sources=StripeService.get_transaction_source_count(db, user_id),
        )

    @staticmethod
    def sync_prices_from_stripe(db: Session) -> list[PriceDetails]:
        """Sync prices from Stripe to the database."""
        # Get all active prices from Stripe
        stripe_prices = stripe.Price.list(active=True, expand=["data.product"])

        # Update or create prices in database
        updated_prices = []

        for stripe_price in stripe_prices.data:
            # Skip prices without products
            if not hasattr(stripe_price, "product") or not stripe_price.product:
                continue

            # Get product metadata
            product = stripe_price.product
            assert isinstance(product, stripe.Product), f"Invalid product: {product}"
            tier = product.metadata.get("tier", "premium")
            max_sources = int(product.metadata.get("max_sources", "5"))

            # Find existing price
            price = (
                db.query(Price).filter(Price.stripe_price_id == stripe_price.id).first()
            )

            assert isinstance(stripe_price.recurring, stripe.Price.Recurring), (
                f"Invalid recurring: {stripe_price.recurring}"
            )
            assert isinstance(stripe_price.unit_amount, int), (
                f"Invalid unit amount: {stripe_price.unit_amount}"
            )

            if price:
                # Update existing price
                price.product_name = product.name
                price.tier = SubscriptionTier(tier)
                price.unit_amount = stripe_price.unit_amount
                price.currency = stripe_price.currency
                price.recurring_interval = stripe_price.recurring.interval
                price.max_sources = max_sources
                price.description = product.description
            else:
                # Create new price
                price = Price(
                    stripe_price_id=stripe_price.id,
                    product_name=product.name,
                    tier=SubscriptionTier(tier),
                    unit_amount=stripe_price.unit_amount,
                    currency=stripe_price.currency,
                    recurring_interval=stripe_price.recurring.interval,
                    max_sources=max_sources,
                    description=product.description,
                )
                db.add(price)

            updated_prices.append(price)

        db.commit()

        # Return all active prices as PriceDetails objects
        result = []
        for price in updated_prices:
            db.refresh(price)
            result.append(
                PriceDetails(
                    id=price.id,
                    name=price.product_name,
                    description=price.description,
                    price=price.unit_amount / 100,  # Convert cents to dollars
                    currency=price.currency,
                    interval=price.recurring_interval,
                    tier=price.tier,
                    max_sources=price.max_sources,
                )
            )

        return result
