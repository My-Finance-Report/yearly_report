import { Container, Tabs, useTabs } from "@chakra-ui/react"
import { useQueryClient } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

import type { UserOut } from "@/client"
import Appearance from "@/components/UserSettings/Appearance"
import ChangePassword from "@/components/UserSettings/ChangePassword"
import DeleteAccount from "@/components/UserSettings/DeleteAccount"
import UserInformation from "@/components/UserSettings/UserInformation"

const tabsConfig = [
  { title: "My profile", component: UserInformation },
  { title: "Password", component: ChangePassword },
  { title: "Appearance", component: Appearance },
  { title: "Danger zone", component: DeleteAccount },
]

export const Route = createFileRoute("/_layout/_logged_in/settings")({
  component: UserSettings,
})

function UserSettings() {
  const queryClient = useQueryClient()
  const tabs = useTabs({
    defaultValue: "0",
  })
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"])
  const finalTabs = currentUser?.is_superuser
    ? tabsConfig.slice(0, 3)
    : tabsConfig

  return (
    <Container
      maxW="lg"
      my={8}
      display="flex"
      flexDirection="column"
      alignItems="center"
      justifyContent="center"
    >
      <Tabs.RootProvider variant="enclosed" value={tabs} w="full">
        <Tabs.List justifyContent="center">
          {finalTabs.map((tab, index) => (
            <Tabs.Trigger key={index.toString()} value={index.toString()}>
              {tab.title}
            </Tabs.Trigger>
          ))}
        </Tabs.List>
        {finalTabs.map((tab, index) => (
          <Tabs.Content key={index.toString()} value={index.toString()}>
            <tab.component />
          </Tabs.Content>
        ))}
      </Tabs.RootProvider>
    </Container>
  )
}
