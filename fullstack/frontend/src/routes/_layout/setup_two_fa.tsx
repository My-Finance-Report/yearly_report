
import { createFileRoute } from "@tanstack/react-router"
import { 
  Container,
  Loader, 
} from "@chakra-ui/react"

import TwoFactorSetup from "@/components/TwoFactorSetup"
import useAuth from "@/hooks/useAuth"
import { z } from "zod";

const setupTwoFaSearchSchema = z.object({
  tempToken: z.string().optional(),
})



export const Route = createFileRoute("/_layout/setup_two_fa")({
  component: SetupTwoFa,
  validateSearch: (search) => setupTwoFaSearchSchema.parse(search),
})



function SetupTwoFa() {
  const { 
    reset2FAStates 
  } = useAuth()
  const handleBack = () => {
    reset2FAStates();
  };

  const {tempToken} = Route.useSearch();


  if (!tempToken) {
    return (
      <Container maxW="sm" py={8}>
        <Loader/>
      </Container>
    );
  }

  return (
    <Container maxW="sm" py={8}>
        <TwoFactorSetup 
          onComplete={handleBack}
          tempToken={tempToken}
        />
      </Container>

    )
}
