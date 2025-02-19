import { Box, Text, Button, Menu, MenuItem, HStack } from "@chakra-ui/react";
import { useNavigate } from "@tanstack/react-router";
import { FaUserAstronaut } from "react-icons/fa";
import { FiLogOut, FiUser } from "react-icons/fi";

import useAuth from "../../hooks/useAuth";

const UserMenu = () => {
  const { logout } = useAuth();
  const navigate = useNavigate();

  const handleLogout = async () => {
    logout();
  };

  return (
    <Box position="relative" display={{ base: "none", md: "block" }} marginRight={12}>
      <Menu.Root>
        <Menu.Trigger asChild>
          <Button
            aria-label="User Menu"
            bg="ui.main"
            color="white"
            data-testid="user-menu"
            _hover={{ bg: "ui.mainDark" }}
            rounded={24}
          >
            <FaUserAstronaut />
          </Button>
        </Menu.Trigger>

        <Menu.Content
          placeContent="bottom-center"
          position="absolute"
          minWidth="180px"
          boxShadow="md"
          borderRadius="md"
          zIndex={10}
        >
          <MenuItem value="settings" onClick={() => navigate({ to: "/settings" })}>
            <HStack spaceX={2}>
              <FiUser />
              <Text>My Profile</Text>
            </HStack>
          </MenuItem>
          <MenuItem
            value="logout"
            onClick={handleLogout}
            color="ui.danger"
            fontWeight="bold"
          >
            <HStack spaceX={2}>
              <FiLogOut />
              <Text>Log Out</Text>
            </HStack>
          </MenuItem>
        </Menu.Content>
      </Menu.Root>
    </Box>
  );
};

export default UserMenu;
