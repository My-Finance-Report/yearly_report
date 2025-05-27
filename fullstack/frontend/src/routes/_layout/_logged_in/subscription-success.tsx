import useCustomToast from "@/hooks/useCustomToast";
import {
  Box,
  Button,
  Center,
  Container,
  Heading,
  Icon,
  Spinner,
  Text,
  VStack,
} from "@chakra-ui/react";
import { createFileRoute, useNavigate } from "@tanstack/react-router";
import React, { useEffect, useState } from "react";
import { FaCheckCircle } from "react-icons/fa";
import { z } from "zod";

const subscriptionSearchSchema = z.object({
  session_id: z.string().optional(),
});

export const Route = createFileRoute(
  "/_layout/_logged_in/subscription-success",
)({
  component: SubscriptionSuccess,
  validateSearch: (search) => subscriptionSearchSchema.parse(search),
});

export default function SubscriptionSuccess() {
  const [loading, setLoading] = useState(true);
  const { session_id } = Route.useSearch();
  const toast = useCustomToast();
  const navigate = useNavigate();

  useEffect(() => {
    const verifySubscription = async () => {
      try {
        const sessionId = session_id;
        if (!sessionId) {
          toast("Error", "No session ID found", "error");
          navigate({ to: "/subscription" });
          return;
        }

        //TODO idk what this is
        // Verify the session with the backend if needed
        // This step is optional as Stripe webhooks should handle the subscription creation
        // await api.post('/subscription/verify-session', { session_id: sessionId });

        // Wait a moment to ensure webhook processing
        setTimeout(() => {
          setLoading(false);
        }, 2000);
      } catch {
        toast("Error", "Failed to verify subscription", "error");
        navigate({ to: "/subscription" });
      }
    };

    verifySubscription();
  }, [session_id, toast, navigate]);

  if (loading) {
    return (
      <Center h="100vh">
        <Spinner size="xl" />
      </Center>
    );
  }

  return (
    <Container maxW="container.md" py={16}>
      <VStack spaceY={8} textAlign="center">
        <Icon as={FaCheckCircle} w={20} h={20} color="green.500" />

        <Heading as="h1" size="xl">
          Subscription Successful!
        </Heading>

        <Text fontSize="lg">
          Thank you for subscribing to our service. Your subscription has been
          activated successfully.
        </Text>

        <Box pt={8}>
          <Button
            colorScheme="blue"
            size="lg"
            onClick={() => navigate({ to: "/subscription" })}
          >
            View Subscription Details
          </Button>
        </Box>
      </VStack>
    </Container>
  );
}
