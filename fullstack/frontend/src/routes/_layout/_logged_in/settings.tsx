import { Box, Container } from "@chakra-ui/react"
import { useQueryClient } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

import type { UserOut } from "@/client"
import AdvancedFeatures from "@/components/UserSettings/AdvancedFeatures"
import ChangePassword from "@/components/UserSettings/ChangePassword"
import DeleteAccount from "@/components/UserSettings/DeleteAccount"
import UserInformation from "@/components/UserSettings/UserInformation"

export const Route = createFileRoute("/_layout/_logged_in/settings")({
  component: UserSettings,
})

function UserSettings() {
  const queryClient = useQueryClient()
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"])

  const deleteWorking = false

  return (
    <Container maxW="lg" my={8} display="flex" flexDirection="column">
      <Box mb={8}>
        <UserInformation />
      </Box>

      <Box mb={8} pt={4} borderTop="1px solid" borderColor="gray.200">
        <AdvancedFeatures />
      </Box>

      <Box mb={8} pt={4} borderTop="1px solid" borderColor="gray.200">
        <ChangePassword />
      </Box>

      {!currentUser?.is_superuser && deleteWorking && (
        <Box pt={4} borderTop="1px solid" borderColor="gray.200">
          <DeleteAccount />
        </Box>
      )}
    </Container>
  )
}
