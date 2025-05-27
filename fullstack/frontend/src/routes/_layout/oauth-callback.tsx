import { OauthService } from "@/client";
import { Center, Container, Text, VStack } from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import {
  type NavigateFn,
  createFileRoute,
  useNavigate,
} from "@tanstack/react-router";
import { useEffect } from "react";
export const Route = createFileRoute("/_layout/oauth-callback")({
  component: OAuthCallback,
});

export interface Response2FA {
  requires_2fa_setup: boolean;
  requires_2fa: boolean;
  temp_token: string | null;
}

//export for local helper
export function handleOAuthResponse(
  response: Response2FA,
  navigate: NavigateFn,
) {
  if (response.requires_2fa_setup) {
    navigate({
      to: "/setup_two_fa",
      search: { tempToken: response.temp_token },
    });
    return;
  }
  if (response.requires_2fa) {
    navigate({
      to: "/input_two_fa",
      search: { tempToken: response.temp_token },
    });
    return;
  }

  navigate({ to: "/" });
}

function OAuthCallback() {
  const navigate = useNavigate();
  const params = new URLSearchParams(window.location.search);
  const code = params.get("code");
  const error = params.get("error");
  const state = params.get("state");

  const query = useQuery({
    queryKey: ["oauth-callback", code],
    queryFn: () => OauthService.googleCallback({ code: code! }),
    enabled: !!code && !error && !!state,
  });

  useEffect(() => {
    if (error || !code || !state) {
      navigate({ to: "/login" });
      return;
    }
    if (!query.isSuccess) return;

    handleOAuthResponse(query.data as Response2FA, navigate);
  }, [code, error, state, query.isSuccess, query.data, navigate]);

  return (
    <Container h="100vh">
      <Center h="100%">
        <VStack>
          <Text>Completing authentication...</Text>
        </VStack>
      </Center>
    </Container>
  );
}

export default OAuthCallback;
