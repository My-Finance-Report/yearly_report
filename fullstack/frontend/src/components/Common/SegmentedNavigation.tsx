import { UserOut } from "@/client";
import { SegmentedControl } from "@/components/ui/segmented-control";
import { Box, Flex, HStack, Text, Button } from "@chakra-ui/react";
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
} from "@/components/ui/drawer";
import { useQueryClient } from "@tanstack/react-query";
import { useNavigate, useRouterState } from "@tanstack/react-router";
import {
  FiBriefcase,
  FiDollarSign,
  FiHome,
  FiList,
  FiSettings,
  FiUsers,
  FiMenu,
  FiChevronRight,
} from "react-icons/fi";
import { useState } from "react";
import { useIsMobile } from "@/hooks/useIsMobile";

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiList },
  { value: "/upload-files", label: "Uploads", icon: FiBriefcase },
  { value: "/budget", label: "Budget", icon: FiDollarSign },
  { value: "/settings", label: "User Settings", icon: FiSettings },
];

export function SegmentedNavigation() {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const location = useRouterState().location;
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"]);
  const isMobile = useIsMobile();

  // Example: final nav items + "Admin" if user is superuser
  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : currentUser
      ? navigationItems
      : [];

  const isDemo = location.pathname.startsWith("/demo");

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
          bgColor="#5F62F6"
          color="white"
          px={4}
          py={2}
          direction="column"
          gap={2}
          fontWeight="semibold"
          alignItems="center"
          justifyContent="center"
        >
          <Text fontSize={20} fontWeight={500}>Want a visual breakdown of your income, expenses and trends — like this?</Text>
          <Flex direction={isMobile ? "column" : "row"} gap={3}>
            <Button
              variant="outline"
              color={"#5F62F6"}
              bgColor={"white"}
              borderColor={"#5F62F6"}
              borderRadius={200}
              onClick={() => navigate({ to: "/landing" })}
            >
              How does it work? <FiChevronRight />
            </Button>
            <a target="_blank" href="https://cal.com/matt-carroll">
              <Button variant="outline" bgColor={"white"} color={"#5F62F6"} borderColor={"#5F62F6"} borderRadius={200}>
                Schedule a call with me 
                <FiChevronRight />
              </Button>
            </a>
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
                navigate({ to: newValue.value });
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
              <Flex gap={2}>
            <Button
              variant="outline"
              color={"#5F62F6"}
              bgColor={"white"}
              borderRadius={200}
                onClick={() => navigate({ to: "/login" })}
            >
                Log in
              </Button>
          <Button
              variant="solid"
              color={"white"}
              bgColor={"#5F62F6"}
              borderRadius={200}
                onClick={() => navigate({ to: "/login" })}
            >
              Sign Up
              </Button>
              </Flex>
            )}
          </>
        )}
      </Flex>
    </Flex>
  );
}

function MobileMenu({
  user,
  navigate,
  finalItems,
}: {
  user: UserOut | undefined;
  navigate: (to: { to: string }) => void;
  finalItems: typeof navigationItems;
}) {
  const [open, setOpen] = useState(false);

  return (
    <DrawerRoot
      open={open}
      onOpenChange={(e) => setOpen(e.open)}
      placement={"bottom"}
    >
      <DrawerBackdrop />
      <DrawerTrigger asChild>
        <Button variant="outline" size="sm">
          <FiMenu />
        </Button>
      </DrawerTrigger>
      <DrawerContent>

          <DrawerHeader>
        {user ? (
            <DrawerTitle>{user.full_name}</DrawerTitle>
        ):
        (
        <Flex gap={2}>
            <Button
              variant="outline"
              color={"#5F62F6"}
              bgColor={"white"}
              borderRadius={200}
                onClick={() => navigate({ to: "/login" })}
            >
                Log in
              </Button>
          <Button
              variant="solid"
              color={"white"}
              bgColor={"#5F62F6"}
              borderRadius={200}
                onClick={() => navigate({ to: "/login" })}
            >
              Sign Up
              </Button>
              </Flex>
            )}
          </DrawerHeader>
        <DrawerBody>
          {finalItems.map(({ value, label, icon }) => (
            <Button
              key={value}
              variant="outline"
              size="sm"
              onClick={() => {
                navigate({ to: value });
                setOpen(false);
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
  );
}
