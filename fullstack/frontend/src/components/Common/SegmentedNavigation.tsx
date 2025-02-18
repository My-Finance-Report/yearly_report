import { HStack } from "@chakra-ui/react"
import { SegmentedControl } from "@/components/ui/segmented-control"
import { FiHome, FiSettings, FiBriefcase, FiUsers } from "react-icons/fi"
import { useQueryClient } from "@tanstack/react-query"
import { useNavigate, useMatchRoute } from "@tanstack/react-router"

import type { UserOut } from "../../client"

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiSettings },
  { value: "/upload-files", label: "Uploads", icon: FiBriefcase },
  { value: "/settings", label: "Settings", icon: FiSettings },
]

export function SegmentedNavigation() {
  const queryClient = useQueryClient()
  const navigate = useNavigate()
  const matchRoute = useMatchRoute()
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"])

  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : navigationItems

  const activeValue =
    finalItems.find(({ value }) => matchRoute({ to: value }))?.value || "/transactions"

  return (
    <SegmentedControl
      defaultValue={activeValue}
      items={finalItems.map(({ value, label, icon }) => ({
        value,
        label: (
          <HStack>
            {icon()}
            {label}
          </HStack>
        ),
      }))}
      onValueChange={(value) => {console.log(value); navigate({ to: value.value })}}
    />
  )
}

