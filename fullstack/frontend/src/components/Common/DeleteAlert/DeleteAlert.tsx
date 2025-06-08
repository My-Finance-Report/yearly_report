import { Box, Button, Dialog, Portal, Checkbox } from "@chakra-ui/react";
import { EntityKind, EntityType } from "./types";

import { useMutation, useQueryClient } from "@tanstack/react-query";
import React, { useState } from "react";
import { useForm } from "react-hook-form";

import { NoCodeService, TransactionsService, UsersService } from "@/client";
import useCustomToast from "@/hooks/useCustomToast";

type DeleteProps = {
  isOpen: boolean;
  onClose: () => void;
  entity: EntityType;
  keysToInvalidate?: string[];
};

const STORAGE_KEY_PREFIX = "skip_confirmation_";

function skipConfirmation(type: string) {
  const storageKey = `${STORAGE_KEY_PREFIX}${type}`;
  const skipConfirmation = localStorage.getItem(storageKey) === "true";
  return skipConfirmation;
}

const Delete = ({ isOpen, onClose, entity, keysToInvalidate }: DeleteProps) => {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();
  const cancelRef = React.useRef<HTMLButtonElement | null>(null);
  const [dontShowAgain, setDontShowAgain] = useState(false);
  const {
    handleSubmit,
    formState: { isSubmitting },
  } = useForm();

  React.useEffect(() => {
    if (skipConfirmation(entity.kind) && isOpen) {
      onClose();
      handleDelete();
    }
  }, [isOpen]);

  const deleteEntity = async () => {
    switch (entity.kind) {
      case EntityKind.User:
        await UsersService.deleteUserMe();
        break;
      case EntityKind.Transaction:
        await TransactionsService.deleteTransaction({
          transactionId: entity.id,
        });
        break;
      case EntityKind.Notification:
        await NoCodeService.deleteEffect({
          effectId: entity.id,
        });
        break;
      case EntityKind.Widget:
        await NoCodeService.removeWidget({
          widgetId: entity.id,
          canvasId: entity.canvasId,
        });
        break;
      default:
        throw new Error(`Unexpected`);
    }
  };

  const mutation = useMutation({
    mutationFn: deleteEntity,
    onSuccess: () => {
      showToast(
        "Success",
        `The ${entity.kind.toLowerCase()} was deleted successfully.`,
        "success",
      );
      onClose();
    },
    onError: (error) => {
      console.error("Failed to delete entity", error);
      showToast(
        "An error occurred.",
        `An error occurred while deleting the ${entity.kind.toLowerCase()}.`,
        "error",
      );
    },
    onSettled: () => {
      queryClient.invalidateQueries({
        queryKey: keysToInvalidate,
      });
    },
    onMutate: () => {
      queryClient.cancelQueries({
        queryKey: keysToInvalidate,
      });
    },
  });

  const handleDelete = () => {
    mutation.mutate();
  };

  const onSubmit = async () => {
    if (dontShowAgain) {
      const storageKey = `${STORAGE_KEY_PREFIX}${entity.kind}`;
      localStorage.setItem(storageKey, "true");
    }

    handleDelete();
  };

  if (skipConfirmation(entity.kind) && isOpen) {
    return null;
  }

  return (
    <Dialog.Root role="alertdialog" open={isOpen} onExitComplete={onClose}>
      <Portal>
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content as="form" onSubmit={handleSubmit(onSubmit)}>
            <Dialog.Header>Delete {entity.kind}</Dialog.Header>
            <Dialog.Body>
              {entity.kind === EntityKind.User && (
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
