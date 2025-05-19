import { BsThreeDotsVertical } from "react-icons/bs";
import { FiTrash } from "react-icons/fi";
import { RiSeedlingLine } from "react-icons/ri";
import React from "react";

import { AdminService } from "@/client";
import Delete from "./DeleteAlert";
import { Menu, useDisclosure } from "@chakra-ui/react";
import useCustomToast from "@/hooks/useCustomToast";
import { useMutation } from "@tanstack/react-query";

interface ActionsMenuProps {
  type: "user" | "transaction";
  disabled?: boolean;
  entity: { id: number };
}

interface MenuItemType {
  label: string;
  onClick: () => void | Promise<void>;
  _icon: React.ReactElement;
  color?: string;
}

const ActionsMenu = ({ type,  entity }: ActionsMenuProps) => {
  const deleteModal = useDisclosure();
  const toast = useCustomToast();

  const reseedMutation = useMutation({
    mutationFn: () => AdminService.reseedAccountPage({ userId: entity.id }),
    onSuccess: () => {
      toast("Account page reseeded successfully", "", "success");
    },
    onError: (error: Error) => {
      toast("Failed to reseed account page", error.message, "error");
    },
  });

  const menuItems: Record<ActionsMenuProps["type"], MenuItemType[]> = {
    user: [
      {
        label: "Reseed Account Page",
        onClick: reseedMutation.mutate,
        _icon: <RiSeedlingLine />,
      },
      {
        label: "Delete",
        onClick: deleteModal.onOpen,
        _icon: <FiTrash />,
        color: "ui.danger",
      },
    ],
    transaction: [
      {
        label: "Delete",
        onClick: deleteModal.onOpen,
        _icon: <FiTrash />,
        color: "ui.danger",
      },
    ],
  };



  return (
    <>
      <Menu.Root>
        <Menu.Trigger>
          <BsThreeDotsVertical />
        </Menu.Trigger>
        <Menu.Content>
          {menuItems[type].map((item, index) => (
            <Menu.Item
              key={index}
              value={item.label}
              onClick={item.onClick}
              color={item.color}
            >
              {item.label}
            </Menu.Item>
          ))}
        </Menu.Content>
      </Menu.Root>
      <Delete
        isOpen={deleteModal.open}
        onClose={deleteModal.onClose}
        entity={entity}
        type={type}
      />
    </>
  );
};

export default ActionsMenu;
