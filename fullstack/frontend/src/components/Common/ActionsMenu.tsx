import { Button, Menu, useDisclosure } from "@chakra-ui/react"
import { BsThreeDotsVertical } from "react-icons/bs"
import { FiTrash } from "react-icons/fi"

import Delete from "./DeleteAlert"

interface ActionsMenuProps {
  type: string
  disabled?: boolean
}

const ActionsMenu = ({ type, disabled }: ActionsMenuProps) => {
  const deleteModal = useDisclosure()

  return (
    <>
      <Menu.Root>
        <Button disabled={disabled} as={Button} variant="outline">
          <BsThreeDotsVertical />
        </Button>
        <Menu.Item onClick={deleteModal.onOpen} color="ui.danger" value={type}>
          <FiTrash fontSize="16px" />
          Delete {type}
        </Menu.Item>
        <Delete
          type={type}
          isOpen={deleteModal.open}
          onClose={deleteModal.onClose}
        />
      </Menu.Root>
    </>
  )
}

export default ActionsMenu
