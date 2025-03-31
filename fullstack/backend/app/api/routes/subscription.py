from fastapi import APIRouter, Depends, Header, HTTPException, Request
from sqlalchemy.orm import Session

from app.core.config import settings
from app.db import get_current_user, get_db
from app.local_types import Message
from app.models import Price, SubscriptionTier, User
from app.schemas.subscription import (
    CheckoutSession,
    PriceDetails,
    SubscriptionDetails,
    SubscriptionLimits,
)
from app.stripe.service import StripeService

router = APIRouter(prefix="/subscription", tags=["subscription"])


@router.get("/status", response_model=SubscriptionDetails)
def get_subscription_status(
    current_user: User = Depends(get_current_user), db: Session = Depends(get_db)
) -> SubscriptionDetails:
    """
    Get the current user's subscription status.
    """
    return StripeService.get_subscription_details(db, current_user.id)


@router.get("/check-limits", response_model=SubscriptionLimits)
def check_subscription_limits(
    current_user: User = Depends(get_current_user), db: Session = Depends(get_db)
) -> SubscriptionLimits:
    """
    Check if the user has reached their subscription limits.
    """
    return StripeService.check_subscription_limits(db, current_user.id)


@router.get("/plans", response_model=list[PriceDetails])
def get_subscription_plans(db: Session = Depends(get_db)) -> list[PriceDetails]:
    """
    Get available subscription plans.
    """
    # Get all active prices
    prices = db.query(Price).filter(Price.active).all()

    # Format prices for frontend
    formatted_prices = []
    for price in prices:
        formatted_prices.append(
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

    # Add free tier
    formatted_prices.append(
        PriceDetails(
            id=0,
            name="Free",
            description="Basic plan with limited features",
            price=0,
            currency="usd",
            interval="month",
            tier=SubscriptionTier.free,
            max_sources=1,
        )
    )

    return formatted_prices


@router.post("/checkout", response_model=CheckoutSession)
def create_checkout_session(
    price_id: int,
    current_user: User = Depends(get_current_user),
    db: Session = Depends(get_db),
) -> CheckoutSession:
    """
    Create a checkout session for a subscription.
    """
    # Get the price
    price = db.query(Price).filter(Price.id == price_id).first()
    if not price:
        raise HTTPException(status_code=404, detail="Price not found")

    # Create checkout session
    success_url = f"{settings.SERVER_HOST}/subscription/success?session_id={{CHECKOUT_SESSION_ID}}"
    cancel_url = f"{settings.SERVER_HOST}/subscription/cancel"

    return StripeService.create_checkout_session(
        db, current_user.id, price.id, success_url, cancel_url
    )


@router.post("/cancel")
def cancel_subscription(
    current_user: User = Depends(get_current_user), db: Session = Depends(get_db)
) -> Message:
    """
    Cancel the current user's subscription at the end of the billing period.
    """
    try:
        StripeService.cancel_subscription(db, current_user.id)
        return Message(
            message="Subscription will be canceled at the end of the billing period"
        )
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/webhook")
async def stripe_webhook(
    request: Request,
    stripe_signature: str = Header(None),
    db: Session = Depends(get_db),
) -> dict[str, str]:
    """
    Handle Stripe webhook events.
    """
    # Read the request body
    payload = await request.body()

    try:
        # Process the webhook event
        result = StripeService.handle_webhook_event(db, payload, stripe_signature)
        return result
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/sync-prices")
def sync_prices(
    current_user: User = Depends(get_current_user), db: Session = Depends(get_db)
) -> Message:
    """
    Sync prices from Stripe to the database.
    Admin only endpoint.
    """
    if not current_user.is_superuser:
        raise HTTPException(status_code=403, detail="Not enough permissions")

    StripeService.sync_prices_from_stripe(db)
    return Message(message="Prices synced successfully")
