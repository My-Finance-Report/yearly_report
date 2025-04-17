"use client"

import {
  Portal,
  Dialog,
  Button,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { useForm } from "react-hook-form"

import { type ApiError, UsersService } from "../../client"
import useAuth from "../../hooks/useAuth"
import useCustomToast from "../../hooks/useCustomToast"
import { handleError } from "../../utils"

interface DeleteProps {
  isOpen: boolean
  onClose: () => void
}

const DeleteConfirmation = ({ isOpen, onClose }: DeleteProps) => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()
  const {
    handleSubmit,
    formState: { isSubmitting },
  } = useForm()
  const { logout } = useAuth()

  const mutation = useMutation({
    mutationFn: () => UsersService.deleteUserMe(),
    onSuccess: () => {
      showToast(
        "Success",
        "Your account has been successfully deleted.",
        "success",
      )
      logout()
      onClose()
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["currentUser"] })
    },
  })

  const onSubmit = async () => {
    mutation.mutate()
  }

  return (
    <Dialog.Root
    open={isOpen}
    onExitComplete={onClose}
    role="alertdialog"
  >
    <Portal>
    <Dialog.Backdrop />
    <Dialog.Positioner>
      <Dialog.Content as="form" onSubmit={handleSubmit(onSubmit)}>
        <Dialog.Header>Delete User</Dialog.Header>
        <Dialog.Body>
            <span>
              All items associated with this user will also be{" "}
              <strong>permanently deleted. </strong>
            </span>
          Are you sure? You will not be able to undo this action.
        </Dialog.Body>

        <Dialog.Footer gap={3}>
          <Button colorScheme="red" type="submit" loading={isSubmitting}>
            Delete
          </Button>
          <Button variant='outline' onClick={onClose} disabled={isSubmitting}>
            Cancel
          </Button>
        </Dialog.Footer>
      </Dialog.Content>
    </Dialog.Positioner>
    </Portal>
  </Dialog.Root>
  )
}
 
export default DeleteConfirmation
