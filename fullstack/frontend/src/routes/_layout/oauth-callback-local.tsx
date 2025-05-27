import { OauthService } from "@/client";
import { Center, Container, Spinner, Text, VStack } from "@chakra-ui/react";
import { createFileRoute, redirect, useNavigate } from "@tanstack/react-router";
import { useEffect, useState } from "react";
import useCustomToast from "../../hooks/useCustomToast";
import { type Response2FA, handleOAuthResponse } from "./oauth-callback";
export const Route = createFileRoute("/_layout/oauth-callback-local")({
  component: OAuthCallbackLocal,
  beforeLoad: async () => {
    if (import.meta.env.DEV) {
      throw redirect({ to: "/" });
    }
  },
});

function OAuthCallbackLocal() {
  const navigate = useNavigate();
  const showToast = useCustomToast();
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const exchangeCodeForToken = async () => {
      try {
        const response = await OauthService.googleCallback({ code: "1" }); //make this local for local testing
        return handleOAuthResponse(response as Response2FA, navigate);
      } catch (error) {
        console.error("OAuth callback error:", error);
        showToast(
          "Authentication Error",
          "Failed to complete authentication",
          "error",
        );
        navigate({ to: "/login" });
      } finally {
        setIsLoading(false);
      }
    };

    exchangeCodeForToken();
  }, []);

  return (
    <Container h="100vh">
      <Center h="100%">
        <VStack>
          {isLoading ? (
            <>
              <Spinner size="xl" />
              <Text>Completing authentication...</Text>
            </>
          ) : (
            <Text>Redirecting...</Text>
          )}
        </VStack>
      </Center>
    </Container>
  );
}

export default OAuthCallbackLocal;
