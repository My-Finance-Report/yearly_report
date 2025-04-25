"use client";

import {
  Button,
  Checkbox,
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogRoot,
  DialogTitle,
  Field,
  Flex,
  Input,
} from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { type SubmitHandler, useForm } from "react-hook-form";

import { type UserRegister, UsersService } from "../../client";
import type { ApiError } from "../../client/core/ApiError";
import useCustomToast from "../../hooks/useCustomToast";
import { emailPattern, handleError } from "../../utils";

interface AddUserProps {
  isOpen: boolean;
  onClose: () => void;
}

interface UserCreateForm extends UserRegister {
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
    mutationFn: (data: UserRegister) =>
      UsersService.createUser({ requestBody: data }),
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
      <DialogContent as="form" onSubmit={handleSubmit(onSubmit)}>
        <DialogHeader>
          <DialogTitle>Add User</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <Field.Root invalid={!!errors.email} required>
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
            {errors.email && (
              <Field.ErrorText>{errors.email.message}</Field.ErrorText>
            )}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.full_name}>
            <Field.Label htmlFor="name">Full name</Field.Label>
            <Input
              id="name"
              {...register("full_name")}
              placeholder="Full name"
              type="text"
            />
            {errors.full_name && (
              <Field.ErrorText>{errors.full_name.message}</Field.ErrorText>
            )}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.password} required>
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
            {errors.password && (
              <Field.ErrorText>{errors.password.message}</Field.ErrorText>
            )}
          </Field.Root>

          <Field.Root mt={4} invalid={!!errors.confirm_password} required>
            <Field.Label htmlFor="confirm_password">
              Confirm Password
            </Field.Label>
            <Input
              id="confirm_password"
              {...register("confirm_password", {
                required: "Please confirm your password",
                validate: (value) =>
                  value === getValues().password ||
                  "The passwords do not match",
              })}
              placeholder="Password"
              type="password"
            />
            {errors.confirm_password && (
              <Field.ErrorText>
                {errors.confirm_password.message}
              </Field.ErrorText>
            )}
          </Field.Root>

          <Flex mt={4} gap={4}>
            <Checkbox.Root {...register("is_superuser")} colorScheme="teal">
              Is superuser?
            </Checkbox.Root>
            <Checkbox.Root {...register("is_active")} colorScheme="teal">
              Is active?
            </Checkbox.Root>
          </Flex>
        </DialogBody>

        <DialogFooter gap={3}>
          <Button variant="outline" type="submit" loading={isSubmitting}>
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
