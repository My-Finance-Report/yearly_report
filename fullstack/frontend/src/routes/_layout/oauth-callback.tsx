import { useEffect } from "react"
import { createFileRoute, NavigateFn, redirect, useNavigate } from "@tanstack/react-router"
import { Container, Text, Center, VStack } from "@chakra-ui/react"
import { OauthService } from "@/client"
export const Route = createFileRoute("/_layout/oauth-callback")({
  component: OAuthCallback,
})

export interface Response2FA {
  requires_2fa_setup: boolean
  requires_2fa: boolean
  temp_token: string | null
}

//export for local helper
export function handleOAuthResponse(response: Response2FA, navigate: NavigateFn) {
        if (response.requires_2fa_setup) {
          navigate({ to: "/setup_two_fa" , search: { tempToken: response.temp_token } });
          return;
        }
        if (response.requires_2fa) {
          redirect({ to: "/input_two_fa" , search: { tempToken: response.temp_token } });
          return
        }

        navigate({ to: "/" })

}


function OAuthCallback() {
  const navigate = useNavigate()
  //const showToast = useCustomToast()


  useEffect(() => {
    const exchangeCodeForToken = async () => {
      console.log("Exchange code for token")
      try {
        const params = new URLSearchParams(window.location.search)
        const code = params.get("code")
        const error = params.get("error")
        const state = params.get("state")
        
        if (error) {
          //showToast("Authentication Error", error, "error")
          navigate({ to: "/login" })
          return
        }
        
        if (!code) {
          //showToast("Authentication Error", "No authorization code received", "error")
          navigate({ to: "/login" })
          return
        }
        
        if (!state) {
          //showToast("Authentication Error", "No state received", "error")
          navigate({ to: "/login" })
          return
        }
        
        // Exchange the code for a token with our backend
        const response = await OauthService.googleCallback({code})
        handleOAuthResponse(response as Response2FA, navigate) //todo typing
      } catch (error) {
        console.error("OAuth callback error:", error)
        //showToast("Authentication Error", "Failed to complete authentication", "error")
        navigate({ to: "/login" })
      }
    }
    
    exchangeCodeForToken()
  }, [navigate])
  
  return (
    <Container h="100vh">
      <Center h="100%">
        <VStack>
          <Text>Completing authentication...</Text>
        </VStack>
      </Center>
    </Container>
  )
}

export default OAuthCallback
