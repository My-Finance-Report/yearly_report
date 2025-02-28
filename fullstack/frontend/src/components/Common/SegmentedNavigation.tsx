import { SegmentedControl } from "@/components/ui/segmented-control"
import { Box, Flex, HStack, Text } from "@chakra-ui/react"
import { useQueryClient } from "@tanstack/react-query"
import { useNavigate, useRouterState } from "@tanstack/react-router"
import { FiBriefcase, FiHome, FiSettings, FiUsers,FiList } from "react-icons/fi"

import type { UserOut } from "../../client"

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiList },
  { value: "/upload-files", label: "Uploads", icon: FiBriefcase },
  { value: "/settings", label: "User Settings", icon: FiSettings },
]

export function SegmentedNavigation() {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const location = useRouterState().location;
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"]);

  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : navigationItems;

  return (
    <Flex align="center" justify="center" py={4} position="sticky" minH={20} top={0} backgroundColor="black" zIndex={1000}>
      <Text position="absolute" left={6} fontSize="24px" fontWeight="bold">
        My Financé
      </Text>

      <SegmentedControl
        defaultValue="/transactions"
        value={
          finalItems.find(({ value }) => location.pathname.startsWith(value))
            ?.value || null
        }
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
          navigate({ to: value.value });
        }}
      />
    </Flex>
  );
}
