import {
  type PriceDetails,
  type SubscriptionDetails,
  SubscriptionService,
} from "@/client"
import useCustomToast from "@/hooks/useCustomToast"
import {
  Badge,
  Box,
  Button,
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Center,
  Container,
  HStack,
  Heading,
  SimpleGrid,
  Spinner,
  StatHelpText,
  StatLabel,
  StatRoot,
  StatValueText,
  Text,
  VStack,
} from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"
import { format } from "date-fns"
import { useEffect, useState } from "react"
import { FaCheck, FaTimes } from "react-icons/fa"

export const Route = createFileRoute("/_layout/_logged_in/subscription")({
  component: Subscription,
})

export default function Subscription() {
  const [plans, setPlans] = useState<PriceDetails[]>([])
  const [status, setStatus] = useState<SubscriptionDetails | null>(null)
  const [loading, setLoading] = useState(true)
  const [checkoutLoading, setCheckoutLoading] = useState(false)
  const toast = useCustomToast()

  useEffect(() => {
    const fetchData = async () => {
      try {
        // Fetch subscription plans
        const plansResponse = await SubscriptionService.getSubscriptionPlans()
        setPlans(plansResponse)

        // Fetch user's subscription status
        const statusResponse = await SubscriptionService.getSubscriptionStatus()
        setStatus(statusResponse)
      } catch {
        toast("Error", "Failed to load subscription information", "error")
      } finally {
        setLoading(false)
      }
    }

    fetchData()
  }, [toast])

  const handleSubscribe = async (planId: number) => {
    try {
      setCheckoutLoading(true)
      const response = await SubscriptionService.createCheckoutSession({
        priceId: planId,
      })

      window.location.href = response.checkout_url
    } catch {
      toast("Error", "Failed to create checkout session", "error")
    } finally {
      setCheckoutLoading(false)
    }
  }

  const handleCancelSubscription = async () => {
    try {
      await SubscriptionService.cancelSubscription()
      // Refresh subscription status
      const statusResponse = await SubscriptionService.getSubscriptionStatus()
      setStatus(statusResponse)

      toast(
        "Subscription Canceled",
        "Your subscription will be canceled at the end of the billing period",
        "success",
      )
    } catch {
      toast("Error", "Failed to cancel subscription", "error")
    }
  }

  if (loading) {
    return (
      <Center h="100vh">
        <Spinner size="xl" />
      </Center>
    )
  }

  const formatCurrency = (amount: number, currency: string) => {
    return new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: currency.toUpperCase(),
    }).format(amount)
  }

  const formatDate = (dateString: string | null) => {
    if (!dateString) return "N/A"
    return format(new Date(dateString), "MMMM dd, yyyy")
  }

  return (
    <Container maxW="container.xl" py={8}>
      <VStack spaceX={8} align="stretch">
        <Heading as="h1" size="xl">
          Subscription Management
        </Heading>

        {status && (
          <Card.Root>
            <CardHeader>
              <Heading size="md">Current Subscription</Heading>
            </CardHeader>
            <CardBody>
              <SimpleGrid columns={{ base: 1, md: 3 }} spaceX={4}>
                <StatRoot>
                  <StatLabel>Plan</StatLabel>
                  <StatValueText>
                    {status.tier.charAt(0).toUpperCase() + status.tier.slice(1)}{" "}
                    <Badge
                      colorScheme={status.status === "active" ? "green" : "red"}
                    >
                      {status.status}
                    </Badge>
                  </StatValueText>
                </StatRoot>

                <StatRoot>
                  <StatLabel>Transaction Sources</StatLabel>
                  <StatValueText>
                    {status.current_sources} / {status.max_sources}
                  </StatValueText>
                  <StatHelpText>
                    {status.current_sources >= status.max_sources
                      ? "Limit reached"
                      : `${
                          status.max_sources - status.current_sources
                        } remaining`}
                  </StatHelpText>
                </StatRoot>

                {status.current_period_end && (
                  <StatRoot>
                    <StatLabel>Current Period Ends</StatLabel>
                    <StatValueText>
                      {formatDate(status.current_period_end)}
                    </StatValueText>
                    {status.cancel_at_period_end && (
                      <StatHelpText color="red.500">
                        Will not renew
                      </StatHelpText>
                    )}
                  </StatRoot>
                )}
              </SimpleGrid>

              {status.status === "active" &&
                status.tier !== "free" &&
                !status.cancel_at_period_end && (
                  <Box mt={4}>
                    <Button
                      colorScheme="red"
                      variant="outline"
                      onClick={handleCancelSubscription}
                    >
                      Cancel Subscription
                    </Button>
                  </Box>
                )}
            </CardBody>
          </Card.Root>
        )}

        <Box>
          <Heading as="h2" size="lg" mb={4}>
            Available Plans
          </Heading>
          <SimpleGrid columns={{ base: 1, md: 3 }} spaceX={8}>
            {plans.map((plan) => (
              <Card.Root
                key={plan.id}
                borderWidth="1px"
                borderRadius="lg"
                overflow="hidden"
              >
                <CardHeader
                  bg={plan.tier === "premium" ? "blue.500" : "gray.100"}
                  color={plan.tier === "premium" ? "white" : "black"}
                >
                  <Heading size="md">{plan.name}</Heading>
                </CardHeader>
                <CardBody>
                  <VStack spaceX={4} align="stretch">
                    <Box>
                      <Heading size="xl">
                        {formatCurrency(plan.price, plan.currency)}
                        <Text as="span" fontSize="md">
                          /{plan.interval}
                        </Text>
                      </Heading>
                    </Box>
                    <Text>{plan.description}</Text>
                    <Box>
                      <Text fontWeight="bold" mb={2}>
                        Features:
                      </Text>
                      <VStack align="start" spaceX={2}>
                        <HStack>
                          <Box color="green.500">
                            <FaCheck />
                          </Box>
                          <Text>
                            Up to {plan.max_sources} transaction sources
                          </Text>
                        </HStack>
                        <HStack>
                          <Box color="green.500">
                            <FaCheck />
                          </Box>
                          <Text>Transaction categorization</Text>
                        </HStack>
                        {plan.tier === "premium" && (
                          <>
                            <HStack>
                              <Box color="green.500">
                                <FaCheck />
                              </Box>
                              <Text>Advanced analytics</Text>
                            </HStack>
                            <HStack>
                              <Box color="green.500">
                                <FaCheck />
                              </Box>
                              <Text>Priority support</Text>
                            </HStack>
                          </>
                        )}
                        {plan.tier === "free" && (
                          <>
                            <HStack>
                              <Box color="red.500">
                                <FaTimes />
                              </Box>
                              <Text>Advanced analytics</Text>
                            </HStack>
                            <HStack>
                              <Box color="red.500">
                                <FaTimes />
                              </Box>
                              <Text>Priority support</Text>
                            </HStack>
                          </>
                        )}
                      </VStack>
                    </Box>
                  </VStack>
                </CardBody>
                <CardFooter>
                  <Button
                    colorScheme={plan.tier === "premium" ? "blue" : "gray"}
                    width="full"
                    loading={checkoutLoading}
                    disabled={
                      status?.tier === plan.tier ||
                      (status?.tier === "premium" && plan.tier === "free")
                    }
                    onClick={() => handleSubscribe(plan.id)}
                  >
                    {status?.tier === plan.tier
                      ? "Current Plan"
                      : "Select Plan"}
                  </Button>
                </CardFooter>
              </Card.Root>
            ))}
          </SimpleGrid>
        </Box>
      </VStack>
    </Container>
  )
}
