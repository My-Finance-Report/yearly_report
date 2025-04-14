import {
  Box,
  Button,
  Container,
  Heading,
  Icon,
  Text,
  VStack,
} from "@chakra-ui/react"
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import { FaTimesCircle } from "react-icons/fa"

export const Route = createFileRoute("/_layout/_logged_in/subscription-cancel")(
  {
    component: SubscriptionCancel,
  },
)

export default function SubscriptionCancel() {
  const navigate = useNavigate()

  return (
    <Container maxW="container.md" py={16}>
      <VStack spaceX={8} textAlign="center">
        <Icon as={FaTimesCircle} w={20} h={20} color="red.500" />

        <Heading as="h1" size="xl">
          Subscription Checkout Canceled
        </Heading>

        <Text fontSize="lg">
          You've canceled the subscription checkout process. No changes have
          been made to your account.
        </Text>

        <Box pt={8}>
          <Button
            colorScheme="blue"
            size="lg"
            onClick={() => navigate({ to: "/subscription" })}
          >
            Return to Subscription Page
          </Button>
        </Box>
      </VStack>
    </Container>
  )
}
