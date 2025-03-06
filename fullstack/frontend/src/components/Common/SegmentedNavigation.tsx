import { SegmentedControl } from "@/components/ui/segmented-control"
import { Box, Flex, HStack, Spacer, Tag, Text } from "@chakra-ui/react"
import { useQuery, useQueryClient } from "@tanstack/react-query"
import { Link, useNavigate, useRouterState } from "@tanstack/react-router"
import {
  FiBriefcase,
  FiDollarSign,
  FiHome,
  FiList,
  FiSettings,
  FiUsers,
} from "react-icons/fi"

import { UploadsService, type UserOut } from "../../client"

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiList },
  { value: "/upload-files", label: "Uploads", icon: FiBriefcase },
  { value: "/budget", label: "Budget", icon: FiDollarSign },
  { value: "/settings", label: "User Settings", icon: FiSettings },
]

export function SegmentedNavigation() {
  const queryClient = useQueryClient()
  const navigate = useNavigate()
  const location = useRouterState().location
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"])

  const isUploading = false

  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : navigationItems

  return (
    <Flex
      align="center"
      justify="center"
      py={4}
      px={6}
      position="sticky"
      minH={20}
      top={0}
      backgroundColor="background"
      zIndex={1000}
      width="100%"
    >
      <Text
        position="absolute"
        left={6}
        fontSize="24px"
        fontWeight="bold"
        color="colors.ui.main"
      >
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
            <HStack spaceX={2}>
              <Box as={icon} />
              <Text>{label}</Text>
            </HStack>
          ),
        }))}
        onValueChange={(value) => {
          navigate({ to: value.value })
        }}
      />

      {currentUser ? (
        <HStack
          position="absolute"
          onClick={() => navigate({ to: "/settings" })}
          cursor="pointer"
          right={6}
          spaceX={2}
          px={3}
          py={1}
          borderRadius="md"
          backgroundColor="green.600"
        >
          <Box
            width="8px"
            height="8px"
            borderRadius="50%"
            backgroundColor="green.300"
          />
          <Text fontSize="sm" color="white">
            {currentUser?.full_name}
          </Text>
        </HStack>
      ) : (
        <Box />
      )}
    </Flex>
  )
}
