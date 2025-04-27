import { type UserOut, UsersService } from "@/client";
import {
  DrawerActionTrigger,
  DrawerBackdrop,
  DrawerBody,
  DrawerCloseTrigger,
  DrawerContent,
  DrawerFooter,
  DrawerHeader,
  DrawerRoot,
  DrawerTitle,
  DrawerTrigger,
} from "@/components/ui/drawer";
import { SegmentedControl } from "@/components/ui/segmented-control";
import useAuth, { isSessionActive } from "@/hooks/useAuth";
import { useIsMobile } from "@/hooks/useIsMobile";
import {
  Avatar,
  Box,
  Button,
  Flex,
  HStack,
  Menu,
  Portal,
  Separator,
  Text,
} from "@chakra-ui/react";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { useNavigate, useRouterState } from "@tanstack/react-router";
import { useEffect, useState } from "react";
import {
  FiDollarSign,
  FiHome,
  FiList,
  FiMenu,
  FiSettings,
  FiUsers,
} from "react-icons/fi";
import { LuEllipsis } from "react-icons/lu";
import { CollapsibleWorkerStatus } from "./WorkerStatus";

const navigationItems = [
  { value: "/transactions", label: "Dashboard", icon: FiHome },
  { value: "/manage-accounts", label: "Manage Accounts", icon: FiList },
  { value: "/budget", label: "Budget", icon: FiDollarSign },
  { value: "/settings", label: "User Settings", icon: FiSettings },
];

function UserBadge({ currentUser }: { currentUser: UserOut | null }) {
  const { isLoading, logout } = useAuth();
  const navigate = useNavigate();

  if (isLoading) {
    return null;
  }

  return currentUser ? (
    <HStack cursor="pointer" px={3} py={1} borderRadius="md">
      <Avatar.Root onClick={() => navigate({ to: "/settings" })}>
        <Avatar.Fallback name={currentUser?.full_name} />
      </Avatar.Root>
      <Menu.Root>
        <Menu.Trigger asChild>
          <LuEllipsis />
        </Menu.Trigger>
        <Portal>
          <Menu.Positioner>
            <Menu.Content>
              <Menu.Item
                onClick={() => navigate({ to: "/settings" })}
                value="settings"
              >
                User Settings
              </Menu.Item>
              <Menu.Item onClick={() => logout()} value="logout">
                Log out
              </Menu.Item>
            </Menu.Content>
          </Menu.Positioner>
        </Portal>
      </Menu.Root>
    </HStack>
  ) : (
    <Flex gap={2}>
      <Button variant="outline" onClick={() => navigate({ to: "/login" })}>
        Log in
      </Button>
      <Button variant="solid" onClick={() => navigate({ to: "/signup" })}>
        Sign Up
      </Button>
    </Flex>
  );
}

export function SegmentedNavigation() {
  const navigate = useNavigate();
  const location = useRouterState().location;
  const queryClient = useQueryClient();
  const { user: authUser } = useAuth();

  // Use the user from useAuth hook as a fallback
  const { data: currentUser } = useQuery<UserOut | null, Error>({
    queryKey: ["currentUser"],
    queryFn: async () => {
      try {
        return await UsersService.readUserMeOptional();
      } catch {
        return null;
      }
    },
    initialData: authUser || null,
  });

  // Effect to refetch user data when session status changes
  useEffect(() => {
    const handleStorageChange = () => {
      if (isSessionActive()) {
        queryClient.invalidateQueries({ queryKey: ["currentUser"] });
      }
    };

    window.addEventListener("storage", handleStorageChange);

    // Initial check
    if (isSessionActive() && !currentUser) {
      queryClient.invalidateQueries({ queryKey: ["currentUser"] });
    }

    return () => {
      window.removeEventListener("storage", handleStorageChange);
    };
  }, [queryClient, currentUser]);

  const isMobile = useIsMobile();

  // Example: final nav items + "Admin" if user is superuser
  const finalItems = currentUser?.is_superuser
    ? [...navigationItems, { value: "/admin", label: "Admin", icon: FiUsers }]
    : currentUser
      ? navigationItems
      : [];

  // Check if we're running on localhost (development environment)
  const isDevelopment = window.location.hostname === "localhost";

  return (
    <Flex
      direction="column"
      position="sticky"
      top={0}
      zIndex={1000}
      backgroundColor="background"
      width="100%"
    >
      {isDevelopment && (
        <Flex
          bgColor="#5F62F6"
          color="white"
          px={4}
          py={4}
          direction="column"
          gap={2}
          fontWeight="semibold"
          alignItems="center"
          justifyContent="center"
        >
          <Text fontSize={20} fontWeight={500}>
            You cant auth from localhost, it needs to be 127.0.0.1 to match the
            backend because http only cookies
          </Text>
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
        <Text
          cursor="click"
          onClick={() => navigate({ to: "/transactions" })}
          fontSize="20px"
          fontWeight="bold"
          color="colors.ui.main"
        >
          My Financé
        </Text>

        {isMobile ? (
          <MobileMenu
            user={currentUser ?? undefined}
            navigate={navigate}
            finalItems={finalItems}
          />
        ) : (
          <>
            <SegmentedControl
              defaultValue="/transactions"
              value={
                finalItems.find(({ value }) =>
                  location.pathname.startsWith(value),
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
                navigate({
                  to: newValue.value ? newValue.value : "/transactions",
                });
              }}
            />
            <Flex direction={"row"} justifyContent={"center"} gap={3}>
              <CollapsibleWorkerStatus />
              {currentUser && <Separator orientation="vertical" />}
              <UserBadge currentUser={currentUser} />
            </Flex>
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

  // Group navigation items by category
  const mainNavItems = finalItems.filter((item) =>
    ["/transactions", "/manage-accounts", "/budget"].includes(item.value),
  );
  const settingsItems = finalItems.filter((item) =>
    ["/settings"].includes(item.value),
  );
  const adminItems = finalItems.filter((item) =>
    ["/admin"].includes(item.value),
  );

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
            <Flex alignItems="center" gap={2}>
              <Box
                width="10px"
                height="10px"
                borderRadius="50%"
                backgroundColor="green.400"
              />
              <DrawerTitle>{user.full_name}</DrawerTitle>
            </Flex>
          ) : (
            <Flex gap={2}>
              <Button
                variant="outline"
                onClick={() => navigate({ to: "/login" })}
              >
                Log in
              </Button>
              <Button
                variant="solid"
                onClick={() => navigate({ to: "/signup" })}
              >
                Sign Up
              </Button>
            </Flex>
          )}
        </DrawerHeader>
        <DrawerBody>
          {/* Main Navigation */}
          <Box mb={4}>
            <Text fontWeight="medium" mb={2} color="gray.500" fontSize="sm">
              Main Navigation
            </Text>
            <Flex direction="column" gap={2}>
              {mainNavItems.map(({ value, label, icon }) => (
                <Button
                  key={value}
                  variant="ghost"
                  justifyContent="flex-start"
                  onClick={() => {
                    navigate({ to: value });
                    setOpen(false);
                  }}
                >
                  <Flex align="center" gap={2}>
                    <Box as={icon} />
                    <Text>{label}</Text>
                  </Flex>
                </Button>
              ))}
            </Flex>
          </Box>

          <Box mb={4}>
            <Text fontWeight="medium" mb={2} color="gray.500" fontSize="sm">
              Settings
            </Text>
            <Flex direction="column" gap={2}>
              {settingsItems.map(({ value, label, icon }) => (
                <Button
                  key={value}
                  variant="ghost"
                  justifyContent="flex-start"
                  onClick={() => {
                    navigate({ to: value });
                    setOpen(false);
                  }}
                >
                  <Flex align="center" gap={2}>
                    <Box as={icon} />
                    <Text>{label}</Text>
                  </Flex>
                </Button>
              ))}
            </Flex>
          </Box>

          {/* Admin (if applicable) */}
          {adminItems.length > 0 && (
            <Box mb={4}>
              <Text fontWeight="medium" mb={2} color="gray.500" fontSize="sm">
                Administration
              </Text>
              <Flex direction="column" gap={2}>
                {adminItems.map(({ value, label, icon }) => (
                  <Button
                    key={value}
                    variant="ghost"
                    justifyContent="flex-start"
                    onClick={() => {
                      navigate({ to: value });
                      setOpen(false);
                    }}
                  >
                    <Flex align="center" gap={2}>
                      <Box as={icon} />
                      <Text>{label}</Text>
                    </Flex>
                  </Button>
                ))}
              </Flex>
            </Box>
          )}
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
