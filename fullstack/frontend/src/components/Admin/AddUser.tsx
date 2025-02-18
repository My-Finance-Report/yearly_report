"use client";

import {
  Button,
  Checkbox,
  Flex,
  Input,
  DialogRoot,
  DialogBackdrop,
  DialogContent,
  DialogCloseTrigger,
  DialogHeader,
  DialogTitle,
  DialogBody,
  DialogFooter,
  Field,
} from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { type SubmitHandler, useForm } from "react-hook-form";

import { type UserCreate, UsersService } from "../../client";
import type { ApiError } from "../../client/core/ApiError";
import useCustomToast from "../../hooks/useCustomToast";
import { emailPattern, handleError } from "../../utils";

interface AddUserProps {
  isOpen: boolean;
  onClose: () => void;
}

interface UserCreateForm extends UserCreate {
  confirm_password: string;
}

const AddUser = ({ isOpen, onClose }: AddUserProps) => {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();
  const {
    register,
    handleSubmit,
    reset,
    getValues,
    formState: { errors, isSubmitting },
  } = useForm<UserCreateForm>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      email: "",
      full_name: "",
      password: "",
      confirm_password: "",
      is_superuser: false,
      is_active: false,
    },
  });

  const mutation = useMutation({
    mutationFn: (data: UserCreate) => UsersService.createUser({ requestBody: data }),
    onSuccess: () => {
      showToast("Success!", "User created successfully.", "success");
      reset();
      onClose();
    },
    onError: (err: ApiError) => {
      handleError(err, showToast);
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["users"] });
    },
  });

  const onSubmit: SubmitHandler<UserCreateForm> = (data) => {
    mutation.mutate(data);
  };

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose}>
      <DialogBackdrop />
      <DialogContent as="form" onSubmit={handleSubmit(onSubmit)} size="md">
        <DialogHeader>
          <DialogTitle>Add User</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <Field.Root invalid={!!errors.email} isRequired>
            <Field.Label htmlFor="email">Email</Field.Label>
            <Input
              id="email"
              {...register("email", {
                required: "Email is required",
                pattern: emailPattern,
              })}
              placeholder="Email"
              type="email"
            />
            {errors.email && <Field.ErrorText>{errors.email.message}</Field.ErrorText>}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.full_name}>
            <Field.Label htmlFor="name">Full name</Field.Label>
            <Input id="name" {...register("full_name")} placeholder="Full name" type="text" />
            {errors.full_name && <Field.ErrorText>{errors.full_name.message}</Field.ErrorText>}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.password} isRequired>
            <Field.Label htmlFor="password">Set Password</Field.Label>
            <Input
              id="password"
              {...register("password", {
                required: "Password is required",
                minLength: {
                  value: 8,
                  message: "Password must be at least 8 characters",
                },
              })}
              placeholder="Password"
              type="password"
            />
            {errors.password && <Field.ErrorText>{errors.password.message}</Field.ErrorText>}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.confirm_password} isRequired>
            <Field.Label htmlFor="confirm_password">Confirm Password</Field.Label>
            <Input
              id="confirm_password"
              {...register("confirm_password", {
                required: "Please confirm your password",
                validate: (value) =>
                  value === getValues().password || "The passwords do not match",
              })}
              placeholder="Password"
              type="password"
            />
            {errors.confirm_password && <Field.ErrorText>{errors.confirm_password.message}</Field.ErrorText>}
          </Field.Root>

          <Flex mt={4} gap={4}>
            <Checkbox {...register("is_superuser")} colorScheme="teal">
              Is superuser?
            </Checkbox>
            <Checkbox {...register("is_active")} colorScheme="teal">
              Is active?
            </Checkbox>
          </Flex>
        </DialogBody>

        <DialogFooter gap={3}>
          <Button variant="primary" type="submit" isLoading={isSubmitting}>
            Save
          </Button>
          <DialogCloseTrigger asChild>
            <Button>Cancel</Button>
          </DialogCloseTrigger>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  );
};

export default AddUser;
