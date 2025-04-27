import { AccountsService } from "@/client/sdk.gen";
import type { TransactionSourceOut } from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";
import { Button, Flex, Icon, Text, useDisclosure } from "@chakra-ui/react";
import {
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogHeader,
  DialogPositioner,
  DialogRoot,
  DialogTitle,
} from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { FaArchive } from "react-icons/fa";

interface ArchiveButtonProps {
  sourceId: number;
  isArchived: boolean;
}

export function ArchiveButton({ sourceId, isArchived }: ArchiveButtonProps) {
  const { open, onOpen, onClose } = useDisclosure();
  const queryClient = useQueryClient();
  const toast = useCustomToast();

  const toggleArchiveMutation = useMutation({
    mutationFn: () =>
      AccountsService.toggleArchiveTransactionSource({ sourceId }),
    onSuccess: (data: TransactionSourceOut) => {
      queryClient.invalidateQueries({ queryKey: ["accounts"] });
      const action = data.archived ? "archived" : "unarchived";
      toast(
        `Account ${action}`,
        `The account has been ${action} successfully.`,
        "success",
      );
      onClose();
    },
    onError: () => {
      toast(
        "Action failed",
        "There was an error updating the account status.",
        "error",
      );
    },
  });

  const handleToggleArchive = () => {
    toggleArchiveMutation.mutate();
  };

  const buttonText = isArchived ? "Unarchive" : "Archive";
  const confirmText = isArchived
    ? "Are you sure you want to unarchive this account? It will appear in your active accounts list again."
    : "Are you sure you want to archive this account? It will be hidden from your active accounts list.";

  return (
    <>
      <Button
        size="sm"
        variant="outline"
        colorScheme={isArchived ? "green" : "gray"}
        onClick={onOpen}
      >
        <Icon as={FaArchive} mr={2} />
        {buttonText}
      </Button>

      <DialogRoot open={open} onOpenChange={onClose}>
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>{buttonText} Account</DialogTitle>
              <DialogCloseTrigger />
            </DialogHeader>

            <DialogBody>
              <Text mb={4}>{confirmText}</Text>
              <Flex justify="space-between">
                <DialogCloseTrigger asChild>
                  <Button variant="outline">Cancel</Button>
                </DialogCloseTrigger>
                <Button
                  onClick={handleToggleArchive}
                  loading={toggleArchiveMutation.isPending}
                >
                  Confirm {buttonText}
                </Button>
              </Flex>
            </DialogBody>
          </DialogContent>
        </DialogPositioner>
      </DialogRoot>
    </>
  );
}
