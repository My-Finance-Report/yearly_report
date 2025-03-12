import { UserOut } from "@/client"
import { SegmentedControl } from "@/components/ui/segmented-control"
import {
  Box,
  Flex,
  HStack,
  Text,
  Button,
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
import { FiBriefcase, FiDollarSign, FiHome, FiList, FiSettings, FiUsers, FiMenu, FiArrowRight} from "react-icons/fi"
import { useState } from "react"
import { useIsMobile } from "@/hooks/useIsMobile"

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
  const isMobile = useIsMobile()
  
  // Example: final nav items + "Admin" if user is superuser
  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : currentUser
    ? navigationItems
    : []

  const isDemo = location.pathname.startsWith("/demo")

  return (
    <Flex
      direction="column"
      position="sticky"
      top={0}
      zIndex={1000}
      backgroundColor="background"
      width="100%"
    >
      {isDemo && (
        <Flex
          bg="yellow.600"
          rounded="md"
          color="white"
          px={4}
          py={2}
          direction="column"
          gap={2}
          cursor="pointer"
          fontWeight="semibold"
          alignItems="center"
          justifyContent="center"
          onClick={() => navigate({ to: "/signup" })}
        >
            <Text fontWeight={"bold"}>
              Want your finance's to look like this? 
            </Text>
            <Flex direction='column'  alignItems={'start'}>

            <Button variant={'ghost'} >
              1. Create an account
              <FiArrowRight/>
            </Button>
            <Button variant={'ghost'}>
              2. Upload your statements 
            </Button>
            </Flex>
      </Flex>
      )}

      <Flex
        align="center"
        justify="space-between"
        py={4}
        px={6}
        minH={20}
        width="100%"
      >
        <Text fontSize="24px" fontWeight="bold" color="colors.ui.main">
          My Financé
        </Text>

        {isMobile ? (
          <MobileMenu
            user={currentUser}
            navigate={navigate}
            finalItems={finalItems}
          />
        ) : (
          <>
            <SegmentedControl
              defaultValue="/transactions"
              value={
                finalItems.find(({ value }) =>
                  location.pathname.startsWith(value)
                )?.value || null
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
              onValueChange={(newValue) => {
                navigate({ to: newValue.value })
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
    </Flex>
  )
}

function MobileMenu({
  user,
  navigate,
  finalItems,
}: {
  user: UserOut | undefined
  navigate: (to: { to: string }) => void
  finalItems: typeof navigationItems
}) {
  const [open, setOpen] = useState(false)

  return (
    <DrawerRoot open={open} onOpenChange={(e) => setOpen(e.open)} placement={"bottom"}>
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
          {finalItems.map(({ value, label, icon }) => (
            <Button
              key={value}
              variant="outline"
              size="sm"
              onClick={() => {
                navigate({ to: value })
                setOpen(false)
              }}
              mb={2}
            >
              <Box as={icon} />
              {label}
            </Button>
          ))}
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

