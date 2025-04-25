import { Button, Dialog, Portal } from "@chakra-ui/react";

import { useMutation, useQueryClient } from "@tanstack/react-query";
import React from "react";
import { useForm } from "react-hook-form";

import { UsersService } from "../../client";
import useCustomToast from "../../hooks/useCustomToast";

interface DeleteProps {
  type: string;
  isOpen: boolean;
  onClose: () => void;
}

const Delete = ({ type, isOpen, onClose }: DeleteProps) => {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();
  const cancelRef = React.useRef<HTMLButtonElement | null>(null);
  const {
    handleSubmit,
    formState: { isSubmitting },
  } = useForm();

  const deleteEntity = async () => {
    if (type === "User") {
      await UsersService.deleteUserMe();
    } else {
      throw new Error(`Unexpected type: ${type}`);
    }
  };

  const mutation = useMutation({
    mutationFn: deleteEntity,
    onSuccess: () => {
      showToast(
        "Success",
        `The ${type.toLowerCase()} was deleted successfully.`,
        "success",
      );
      onClose();
    },
    onError: () => {
      showToast(
        "An error occurred.",
        `An error occurred while deleting the ${type.toLowerCase()}.`,
        "error",
      );
    },
    onSettled: () => {
      queryClient.invalidateQueries({
        queryKey: [type === "Item" ? "items" : "users"],
      });
    },
  });

  const onSubmit = async () => {
    mutation.mutate();
  };

  return (
    <Dialog.Root open={isOpen} onExitComplete={onClose}>
      <Portal>
        <Dialog.Backdrop />
        <Dialog.Positioner position="absolute">
          <Dialog.Content as="form" onSubmit={handleSubmit(onSubmit)}>
            <Dialog.Header>Delete {type}</Dialog.Header>
            <Dialog.Body>
              {type === "User" && (
                <span>
                  All items associated with this user will also be{" "}
                  <strong>permanently deleted. </strong>
                </span>
              )}
              Are you sure? You will not be able to undo this action.
            </Dialog.Body>

            <Dialog.Footer gap={3}>
              <Button colorScheme="red" type="submit" loading={isSubmitting}>
                Delete
              </Button>
              <Button ref={cancelRef} onClick={onClose} disabled={isSubmitting}>
                Cancel
              </Button>
            </Dialog.Footer>
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </Dialog.Root>
  );
};

export default Delete;
