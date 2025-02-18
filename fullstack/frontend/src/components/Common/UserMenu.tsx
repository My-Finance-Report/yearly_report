import {
  Box,
  IconButton,
  Button,
} from "@chakra-ui/react";
import {
  MenuRoot,
  MenuTrigger,
  MenuContent,
  MenuItem,
} from "@/components/ui/menu";
import { Link } from "@tanstack/react-router";
import { FaUserAstronaut } from "react-icons/fa";
import { FiLogOut, FiUser } from "react-icons/fi";

import useAuth from "../../hooks/useAuth";

const UserMenu = () => {
  const { logout } = useAuth();

  const handleLogout = async () => {
    logout();
  };

  return (
    <>
      {/* Desktop */}
      <Box
        display={{ base: "none", md: "block" }}
        position="fixed"
        top={4}
        right={4}
      >
        <MenuRoot>
          <MenuTrigger asChild>
            <IconButton
              aria-label="Options"
              icon={<FaUserAstronaut color="white" fontSize="18px" />}
              bg="ui.main"
              isRound
              data-testid="user-menu"
            />
          </MenuTrigger>
          <MenuContent>
            <MenuItem asChild>
              <Link to="settings">
                <Button variant="ghost" leftIcon={<FiUser fontSize="18px" />}>
                  My profile
                </Button>
              </Link>
            </MenuItem>
            <MenuItem
              onClick={handleLogout}
              color="ui.danger"
              fontWeight="bold"
              icon={<FiLogOut fontSize="18px" />}
            >
              Log out
            </MenuItem>
          </MenuContent>
        </MenuRoot>
      </Box>
    </>
  );
};

export default UserMenu;
