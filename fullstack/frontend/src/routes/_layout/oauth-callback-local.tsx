import { useEffect, useState } from "react"
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import { Container, Spinner, Text, Center, VStack } from "@chakra-ui/react"
import useCustomToast from "../../hooks/useCustomToast"
import { OauthService } from "@/client"
import { activateSession } from "@/hooks/useAuth"
export const Route = createFileRoute("/_layout/oauth-callback-local")({
  component: OAuthCallback,
})

function handleOAuthResponse(response: Response, navigate) {

  if (response.requires_2fa_setup) {
        navigate({ to: "/setup_two_fa" , search: { tempToken: response.temp_token } });
  }
  if (response.requires_2fa) {
    console.log("sending you to input_two_fa")
    navigate({ to: "/input_two_fa" , search: { tempToken: response.temp_token } });
  }

  return navigate({ to: "/" })
}

function OAuthCallback() {
  const navigate = useNavigate()
  const showToast = useCustomToast()
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    const exchangeCodeForToken = async () => {
      try {
        // Exchange the code for a token with our backend
        const response = await OauthService.googleCallbackLocal({code: "1"})

        handleOAuthResponse(response as unknown as Response, navigate)
      } catch (error) {
        console.error("OAuth callback error:", error)
        showToast("Authentication Error", "Failed to complete authentication", "error")
        navigate({ to: "/login" })
      } finally {
        setIsLoading(false)
      }
    }
    
    exchangeCodeForToken()
  }, [navigate, showToast])
  
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
  )
}

export default OAuthCallback
