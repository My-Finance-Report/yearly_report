import { Button, Menu, useDisclosure } from "@chakra-ui/react"
import { BsThreeDotsVertical } from "react-icons/bs"
import { FiEdit, FiTrash } from "react-icons/fi"

import type { UserOut } from "../../client"
import EditUser from "../Admin/EditUser"
import Delete from "./DeleteAlert"

interface ActionsMenuProps {
  type: string
  value: UserOut
  disabled?: boolean
}

const ActionsMenu = ({ type, value, disabled }: ActionsMenuProps) => {
  const editUserModal = useDisclosure()
  const deleteModal = useDisclosure()

  return (
    <>
      <Menu.Root>
        <Button disabled={disabled} as={Button} variant="outline">
          <BsThreeDotsVertical />
        </Button>
        <Menu.Item value={type} onClick={editUserModal.onOpen}>
          <FiEdit fontSize="16px" />
          Edit {type}
        </Menu.Item>
        <Menu.Item onClick={deleteModal.onOpen} color="ui.danger" value={type}>
          <FiTrash fontSize="16px" />
          Delete {type}
        </Menu.Item>
        {type === "User" ? (
          <EditUser
            user={value as UserOut}
            isOpen={editUserModal.open}
            onClose={editUserModal.onClose}
          />
        ) : null}
        <Delete
          type={type}
          id={value.id}
          isOpen={deleteModal.open}
          onClose={deleteModal.onClose}
        />
      </Menu.Root>
    </>
  )
}

export default ActionsMenu
