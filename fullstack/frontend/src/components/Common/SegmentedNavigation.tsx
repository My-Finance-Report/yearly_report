import { UserOut } from "@/client"
import { SegmentedControl } from "@/components/ui/segmented-control"
import {
  Box,
  Flex,
  HStack,
  Text,
  Button,
  VStack,
  useBreakpointValue
} from "@chakra-ui/react"
import {
  DrawerBackdrop,
  DrawerBody,
  DrawerCloseTrigger,
  DrawerContent,
  DrawerFooter,
  DrawerActionTrigger,
  DrawerHeader,
  DrawerRoot,
  DrawerTitle,
  DrawerTrigger,
} from "@/components/ui/drawer"
import { useQueryClient } from "@tanstack/react-query"
import { useNavigate, useRouterState } from "@tanstack/react-router"
import { FiBriefcase, FiDollarSign, FiHome, FiList, FiSettings, FiUsers, FiMenu } from "react-icons/fi"
import { useState } from "react"

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
  const isMobile = useBreakpointValue({ base: true, md: false })

  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : currentUser ? navigationItems : []

  return (
    <Flex
      align="center"
      justify="space-between"
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
        fontSize="24px"
        fontWeight="bold"
        color="colors.ui.main"
      >
        My Financé
      </Text>
      {isMobile ? (
        <MobileMenu user={currentUser} navigate={navigate} finalItems={finalItems} />
      ) : (
        <>
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
              onClick={() => navigate({ to: "/settings" })}
              cursor="pointer"
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
        </>
      )}

    </Flex>
  )
}


function MobileMenu({ user, navigate, finalItems }: { user: UserOut | undefined, navigate: (to: { to: string }) => void, finalItems: typeof navigationItems }) {
  const [open, setOpen] = useState(false)

  return (
    <DrawerRoot open={open} onOpenChange={(e) => setOpen(e.open)} placement={'bottom'}>
      <DrawerBackdrop />
      <DrawerTrigger asChild>
        <Button variant="outline" size="sm">
          <FiMenu />
        </Button>
      </DrawerTrigger>
      <DrawerContent>
        {user && (
          <DrawerHeader>
            <DrawerTitle>{user.full_name}</DrawerTitle>
          </DrawerHeader>
        )}
        <DrawerBody>
          <VStack align="start" gap={2}>
            {finalItems.map(({ value, label, icon }) => (
              <Button
                key={value}
                variant="outline"
                size="sm"
                onClick={() => {
                  navigate({ to: value })
                  setOpen(false)
                }}
              >
                <Box as={icon} />
                {label}
              </Button>
            ))}
          </VStack>
        </DrawerBody>
        <DrawerFooter>
          <DrawerActionTrigger asChild>
            <Button variant="outline">Close</Button>
          </DrawerActionTrigger>
        </DrawerFooter>
        <DrawerCloseTrigger />
      </DrawerContent>
    </DrawerRoot>
  )
}