import { useEffect, useState } from "react"
import { createFileRoute, redirect, useNavigate } from "@tanstack/react-router"
import { Container, Spinner, Text, Center, VStack } from "@chakra-ui/react"
import useCustomToast from "../../hooks/useCustomToast"
import { OauthService } from "@/client"
import { handleOAuthResponse, Response2FA } from "./oauth-callback"
export const Route =  createFileRoute("/_layout/oauth-callback-local")({
  component: OAuthCallback,
  beforeLoad: async () => {
    if (import.meta.env.DEV) {
      throw redirect({ to: "/" })
    }
  }
})

function OAuthCallback() {
  const navigate = useNavigate()
  const showToast = useCustomToast()
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    const exchangeCodeForToken = async () => {
      try {
        const response = await OauthService.googleCallbackLocal({code: "1"})
        handleOAuthResponse(response as Response2FA, navigate) 

      } catch (error) {
        console.error("OAuth callback error:", error)
        showToast("Authentication Error", "Failed to complete authentication", "error")
        navigate({ to: "/login" })
      } finally {
        setIsLoading(false)
      }
    }
    
    exchangeCodeForToken()
  }, [])
  
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
