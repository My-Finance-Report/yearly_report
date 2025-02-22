import { SegmentedControl } from "@/components/ui/segmented-control"
import { Box, Flex, HStack, Text } from "@chakra-ui/react"
import { useQueryClient } from "@tanstack/react-query"
import { useNavigate, useRouterState } from "@tanstack/react-router"
import { FiBriefcase, FiHome, FiSettings, FiUsers } from "react-icons/fi"

import type { UserOut } from "../../client"
import UserMenu from "./UserMenu"

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiSettings },
  { value: "/upload-files", label: "Uploads", icon: FiBriefcase },
  { value: "/settings", label: "Settings", icon: FiSettings },
]

export function SegmentedNavigation() {
  const queryClient = useQueryClient()
  const navigate = useNavigate()
  const location = useRouterState().location
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"])

  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : navigationItems

  return (
    <Flex align="center" justify="space-between" py={4} mr={24}>
      <Text fontSize="xl" fontWeight="bold" marginLeft={24}>
        My Finance
      </Text>

      <SegmentedControl
        defaultValue={"/transactions"}
        value={finalItems.find(({ value }) => location.pathname.startsWith(value))?.value || "/transactions"}
        items={finalItems.map(({ value, label, icon }) => ({
          value,
          label: (
            <HStack>
              <Box as={icon} />
              {label}
            </HStack>
          ),
        }))}
        onValueChange={(value) => {
          navigate({ to: value.value })
        }}
      />
      <UserMenu />
    </Flex>
  )
}
