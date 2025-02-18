"use client";

import {
  DialogRoot,
  DialogBackdrop,
  DialogContent,
  DialogCloseTrigger,
  DialogHeader,
  DialogTitle,
  DialogBody,
  DialogFooter,
  Alert,
  AlertIndicator,
  AlertContent,
  AlertTitle,
  AlertDescription,
  Button,
} from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { useForm } from "react-hook-form";

import { type ApiError, UsersService } from "../../client";
import useAuth from "../../hooks/useAuth";
import useCustomToast from "../../hooks/useCustomToast";
import { handleError } from "../../utils";

interface DeleteProps {
  isOpen: boolean;
  onClose: () => void;
}

const DeleteConfirmation = ({ isOpen, onClose }: DeleteProps) => {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();
  const {
    handleSubmit,
    formState: { isSubmitting },
  } = useForm();
  const { logout } = useAuth();

  const mutation = useMutation({
    mutationFn: () => UsersService.deleteUserMe(),
    onSuccess: () => {
      showToast("Success", "Your account has been successfully deleted.", "success");
      logout();
      onClose();
    },
    onError: (err: ApiError) => {
      handleError(err, showToast);
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["currentUser"] });
    },
  });

  const onSubmit = async () => {
    mutation.mutate();
  };

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose}>
      <DialogBackdrop />
      <DialogContent as="form" onSubmit={handleSubmit(onSubmit)} size="md">
        <DialogHeader>
          <DialogTitle>Confirmation Required</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <Alert.Root variant="solid" colorScheme="red">
            <AlertIndicator />
            <AlertContent>
              <AlertTitle>Warning</AlertTitle>
              <AlertDescription>
                All your account data will be <strong>permanently deleted.</strong> If you are sure,
                please click <strong>"Confirm"</strong> to proceed. This action cannot be undone.
              </AlertDescription>
            </AlertContent>
          </Alert.Root>
        </DialogBody>

        <DialogFooter gap={3}>
          <Button variant="danger" type="submit" isLoading={isSubmitting}>
            Confirm
          </Button>
          <DialogCloseTrigger asChild>
            <Button disabled={isSubmitting}>Cancel</Button>
          </DialogCloseTrigger>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  );
};

export default DeleteConfirmation;
