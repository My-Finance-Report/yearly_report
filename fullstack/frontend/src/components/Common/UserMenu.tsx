import { Box, Text, Button, Menu, MenuItem, HStack } from "@chakra-ui/react";
import { Link, useNavigate } from "@tanstack/react-router";
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
    <>
      <Box display={{ base: "none", md: "block" }}>
        <Menu.Root>
          <Menu.Trigger asChild>
            <Button
              aria-label="User Menu"
              bg="ui.main"
              color="white"
              data-testid="user-menu"
            >
              <FaUserAstronaut />
            </Button>
          </Menu.Trigger>
          <Menu.Content>
            <MenuItem value="settings"
              onClick={() => navigate({to: "/settings"})}
              >
              <Link title="My Profile" href="/settings">
                <HStack>
                  <FiUser />
                  <Text>My Profile</Text>
                </HStack>
              </Link>
            </MenuItem>
            <MenuItem
              value="logout"
              onClick={handleLogout}
              color="ui.danger"
              fontWeight="bold"
            >
              <HStack>
                <FiLogOut />
                <Text>Log Out</Text>
              </HStack>
            </MenuItem>
          </Menu.Content>
        </Menu.Root>
      </Box>
    </>
  );
};

export default UserMenu;
