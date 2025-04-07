
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import { 
  Container,
  Text, 
} from "@chakra-ui/react"

import useAuth from "@/hooks/useAuth"
import { z } from "zod";
import TwoFactorVerification from "@/components/TwoFactorVerification";
import { useQueryClient } from "@tanstack/react-query";

const inputTwoFaSearchSchema = z.object({
  tempToken: z.string().optional(),
})


export const Route = createFileRoute("/_layout/input_two_fa")({
  component: InputTwoFa,
  validateSearch: (search) => inputTwoFaSearchSchema.parse(search),
})


function InputTwoFa() {

  const { 
    reset2FAStates 
  } = useAuth()
  const handleBack = () => {

    reset2FAStates();
    return navigate({ to: "/login" })
  };

  const queryClient = useQueryClient()
  const navigate = useNavigate()

  const handleSuccessfulVerification = () => {
    sessionStorage.setItem("session_active", "true")
    
    queryClient.invalidateQueries({ queryKey: ["currentUser"] })
    
    reset2FAStates();
    navigate({ to: "/" });
  };



  const {tempToken} = Route.useSearch();


  if (!tempToken) {
    return (
      <Container maxW="sm" py={8}>
        <Text>No temp token found</Text>
      </Container>
    );
  }

  return (
    <Container maxW="sm" py={8}>
        <TwoFactorVerification 
          onCancel={handleBack}
          onSuccess={handleSuccessfulVerification}
          temp_token={tempToken}
        />
      </Container>

    )
}
