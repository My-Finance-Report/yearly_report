import { useEffect, useState } from "react"
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import { Container, Spinner, Text, Center, VStack } from "@chakra-ui/react"
import useCustomToast from "../../hooks/useCustomToast"
import { OauthService } from "@/client"
import { activateSession } from "@/hooks/useAuth"
export const Route = createFileRoute("/_layout/oauth-callback")({
  component: OAuthCallback,
})

function OAuthCallback() {
  const navigate = useNavigate()
  const showToast = useCustomToast()
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    const exchangeCodeForToken = async () => {
      try {
        const params = new URLSearchParams(window.location.search)
        const code = params.get("code")
        const error = params.get("error")
        const state = params.get("state")
        
        if (error) {
          showToast("Authentication Error", error, "error")
          navigate({ to: "/login" })
          return
        }
        
        if (!code) {
          showToast("Authentication Error", "No authorization code received", "error")
          navigate({ to: "/login" })
          return
        }
        
        if (!state) {
          showToast("Authentication Error", "No state received", "error")
          navigate({ to: "/login" })
          return
        }
        
        // Exchange the code for a token with our backend
        await OauthService.googleCallback({code})

        activateSession()

        showToast("Login Successful", "You have successfully signed in with Google", "success")
        navigate({ to: "/" })
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
