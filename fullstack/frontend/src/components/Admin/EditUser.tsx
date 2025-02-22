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
  FieldErrorText,
  FieldLabel,
  FieldRoot,
  Flex,
  Input,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { type SubmitHandler, useForm } from "react-hook-form"

import {
  type ApiError,
  type UserOut,
  type UserUpdate,
  UsersService,
} from "../../client"
import useCustomToast from "../../hooks/useCustomToast"
import { emailPattern, handleError } from "../../utils"

interface EditUserProps {
  user: UserOut
  isOpen: boolean
  onClose: () => void
}

interface UserUpdateForm extends UserUpdate {
  confirm_password: string
}

const EditUser = ({ user, isOpen, onClose }: EditUserProps) => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()

  const {
    register,
    handleSubmit,
    reset,
    getValues,
    formState: { errors, isSubmitting, isDirty },
  } = useForm<UserUpdateForm>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: user,
  })

  const mutation = useMutation({
    mutationFn: (data: UserUpdateForm) =>
      UsersService.updateUser({
        userId: user.id.toString(),
        requestBody: data,
      }),
    onSuccess: () => {
      showToast("Success!", "User updated successfully.", "success")
      onClose()
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["users"] })
    },
  })

  const onSubmit: SubmitHandler<UserUpdateForm> = async (data) => {
    if (data.password === "") {
      data.password = null
    }
    mutation.mutate(data)
  }

  const onCancel = () => {
    reset()
    onClose()
  }

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose}>
      <DialogBackdrop />
      <DialogContent as="form" onSubmit={handleSubmit(onSubmit)}>
        <DialogHeader>
          <DialogTitle>Edit User</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <FieldRoot invalid={!!errors.email} required>
            <FieldLabel htmlFor="email">Email</FieldLabel>
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
              <FieldErrorText>{errors.email.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.full_name}>
            <FieldLabel htmlFor="name">Full name</FieldLabel>
            <Input
              id="name"
              {...register("full_name")}
              placeholder="Full name"
              type="text"
            />
            {errors.full_name && (
              <FieldErrorText>{errors.full_name.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.password}>
            <FieldLabel htmlFor="password">Set Password</FieldLabel>
            <Input
              id="password"
              {...register("password", {
                minLength: {
                  value: 8,
                  message: "Password must be at least 8 characters",
                },
              })}
              placeholder="Password"
              type="password"
            />
            {errors.password && (
              <FieldErrorText>{errors.password.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.confirm_password}>
            <FieldLabel htmlFor="confirm_password">Confirm Password</FieldLabel>
            <Input
              id="confirm_password"
              {...register("confirm_password", {
                validate: (value) =>
                  value === getValues().password ||
                  "The passwords do not match",
              })}
              placeholder="Confirm Password"
              type="password"
            />
            {errors.confirm_password && (
              <FieldErrorText>{errors.confirm_password.message}</FieldErrorText>
            )}
          </FieldRoot>

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
          <Button
            variant="outline"
            type="submit"
            loading={isSubmitting}
            disabled={!isDirty}
          >
            Save
          </Button>
          <DialogCloseTrigger asChild>
            <Button onClick={onCancel}>Cancel</Button>
          </DialogCloseTrigger>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  )
}
export default EditUser
