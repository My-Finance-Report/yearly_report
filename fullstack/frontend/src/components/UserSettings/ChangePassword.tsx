"use client"

import { Box, Button, Container, Field, Heading, Input } from "@chakra-ui/react"
import { useMutation } from "@tanstack/react-query"
import { type SubmitHandler, useForm } from "react-hook-form"

import { useColorModeValue } from "@/components/ui/color-mode"

import {
  type ApiError,
  UsersService,
  type UsersUpdatePasswordMeData,
} from "../../client"
import useCustomToast from "../../hooks/useCustomToast"
import { confirmPasswordRules, handleError, passwordRules } from "../../utils"

interface UpdatePasswordForm extends UsersUpdatePasswordMeData {
  old_password: string
  new_password: string
  confirm_password: string
}

const ChangePassword = () => {
  const color = useColorModeValue("inherit", "ui.light")
  const showToast = useCustomToast()
  const {
    register,
    handleSubmit,
    reset,
    getValues,
    formState: { errors, isSubmitting },
  } = useForm<UpdatePasswordForm>({
    mode: "onBlur",
    criteriaMode: "all",
  })

  const mutation = useMutation({
    mutationFn: (data: UpdatePasswordForm) =>
      UsersService.updatePasswordMe({ requestBody: data }),
    onSuccess: () => {
      showToast("Success!", "Password updated successfully.", "success")
      reset()
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
  })

  const onSubmit: SubmitHandler<UpdatePasswordForm> = async (data) => {
    mutation.mutate(data)
  }

  return (
    <Container maxW="full">
      <Heading size="sm" py={4}>
        Change Password
      </Heading>
      <Box
        w={{ sm: "full", md: "50%" }}
        as="form"
        onSubmit={handleSubmit(onSubmit)}
      >
        <Field.Root invalid={!!errors.old_password}>
          <Field.Label color={color} htmlFor="current_password">
            Current Password
          </Field.Label>
          <Input
            id="current_password"
            {...register("old_password")}
            placeholder="Password"
            type="password"
            w="auto"
          />
          {errors.old_password && (
            <Field.ErrorText>{errors.old_password.message}</Field.ErrorText>
          )}
        </Field.Root>

        <Field.Root mt={4} invalid={!!errors.new_password}>
          <Field.Label htmlFor="password">Set Password</Field.Label>
          <Input
            id="password"
            {...register("new_password", passwordRules())}
            placeholder="Password"
            type="password"
            w="auto"
          />
          {errors.new_password && (
            <Field.ErrorText>{errors.new_password.message}</Field.ErrorText>
          )}
        </Field.Root>

        <Field.Root mt={4} invalid={!!errors.confirm_password}>
          <Field.Label htmlFor="confirm_password">Confirm Password</Field.Label>
          <Input
            id="confirm_password"
            {...register("confirm_password", confirmPasswordRules(getValues))}
            placeholder="Password"
            type="password"
            w="auto"
          />
          {errors.confirm_password && (
            <Field.ErrorText>{errors.confirm_password.message}</Field.ErrorText>
          )}
        </Field.Root>

        <Button variant="outline" mt={4} type="submit" loading={isSubmitting}>
          Save
        </Button>
      </Box>
    </Container>
  )
}

export default ChangePassword
