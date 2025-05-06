import { Box, Button, Dialog, Portal, Checkbox } from "@chakra-ui/react";

import { useMutation, useQueryClient } from "@tanstack/react-query";
import React, { useState } from "react";
import { useForm } from "react-hook-form";

import {
  TransactionOut,
  TransactionsService,
  UsersService,
} from "../../client";
import useCustomToast from "../../hooks/useCustomToast";

interface DeleteProps {
  type: "user" | "transaction";
  isOpen: boolean;
  onClose: () => void;
  entity?: TransactionOut;
}

const STORAGE_KEY_PREFIX = "skip_confirmation_";

function skipConfirmation(type: string) {
  const storageKey = `${STORAGE_KEY_PREFIX}${type}`;
  const skipConfirmation = localStorage.getItem(storageKey) === "true";
  return skipConfirmation;
}

const Delete = ({ type, isOpen, onClose, entity }: DeleteProps) => {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();
  const cancelRef = React.useRef<HTMLButtonElement | null>(null);
  const [dontShowAgain, setDontShowAgain] = useState(false);
  const {
    handleSubmit,
    formState: { isSubmitting },
  } = useForm();

  React.useEffect(() => {
    if (skipConfirmation(type) && isOpen) {
      onClose();
      handleDelete();
    }
  }, [isOpen, type]);

  const deleteEntity = async () => {
    switch (type) {
      case "user":
        await UsersService.deleteUserMe();
        break;
      case "transaction":
        if (!entity) {
          throw new Error("Entity is required for transaction deletion");
        }
        await TransactionsService.deleteTransaction({
          transactionId: entity.id,
        });
        break;
      default:
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
        queryKey: [type === "user" ? "users" : "aggregatedTransactions"],
      });
    },
    onMutate: () => {
      queryClient.cancelQueries({
        queryKey: [type === "user" ? "users" : "aggregatedTransactions"],
      });
    },
  });

  const handleDelete = () => {
    mutation.mutate();
  };

  const onSubmit = async () => {
    // If user checked "Don't show me again", save to localStorage
    if (dontShowAgain) {
      const storageKey = `${STORAGE_KEY_PREFIX}${type}`;
      localStorage.setItem(storageKey, "true");
    }

    handleDelete();
  };

  // If we should skip confirmation, don't render the dialog at all
  if (skipConfirmation(type) && isOpen) {
    return null;
  }

  return (
    <Dialog.Root role="alertdialog" open={isOpen} onExitComplete={onClose}>
      <Portal>
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content as="form" onSubmit={handleSubmit(onSubmit)}>
            <Dialog.Header>Delete {type}</Dialog.Header>
            <Dialog.Body>
              {type === "user" && (
                <span>
                  All items associated with this user will also be{" "}
                  <strong>permanently deleted. </strong>
                </span>
              )}
              Are you sure? You will not be able to undo this action.
              <Box mt={4} display="flex" alignItems="center">
                <Checkbox.Root
                  checked={dontShowAgain}
                  onCheckedChange={() => setDontShowAgain((prev) => !prev)}
                >
                  <Checkbox.HiddenInput />
                  <Checkbox.Control />
                  <Checkbox.Label>
                    Don't show this confirmation again
                  </Checkbox.Label>
                </Checkbox.Root>
              </Box>
            </Dialog.Body>

            <Dialog.Footer gap={3}>
              <Button ref={cancelRef} onClick={onClose} disabled={isSubmitting}>
                Cancel
              </Button>
              <Button colorPalette="red" type="submit" loading={isSubmitting}>
                Delete
              </Button>
            </Dialog.Footer>
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </Dialog.Root>
  );
};

export default Delete;
