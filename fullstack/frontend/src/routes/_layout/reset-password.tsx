"use client"

import {
  Button,
  Container,
  Field,
  Heading,
  Input,
  Text,
} from "@chakra-ui/react"
import { useMutation } from "@tanstack/react-query"
import { createFileRoute, redirect, useNavigate } from "@tanstack/react-router"
import { RegisterOptions, type SubmitHandler, useForm, UseFormGetValues } from "react-hook-form"

import { type ApiError, LoginService, type NewPassword } from "@/client"
import { isSessionActive } from "@/hooks/useAuth"
import useCustomToast from "@/hooks/useCustomToast"
import { handleError, passwordRules } from "../../utils"

interface NewPasswordForm extends NewPassword {
  confirm_password: string
}

const confirmPasswordRules = (
  getValues: UseFormGetValues<NewPasswordForm>,
  isRequired = true,
) => {
  const rules: RegisterOptions = {
    validate: (value: string) => {
      const password = getValues().new_password || getValues().confirm_password
      return value === password ? true : "The passwords do not match"
    },
  }

  if (isRequired) {
    rules.required = "Password confirmation is required"
  }

  return rules
}

export const Route = createFileRoute("/_layout/reset-password")({
  component: ResetPassword,
  beforeLoad: async () => {
    if (await isSessionActive()) {
      throw redirect({
        to: "/",
      })
    }
  },
})

function ResetPassword() {
  const {
    register,
    handleSubmit,
    getValues,
    reset,
    formState: { errors },
  } = useForm<NewPasswordForm>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      new_password: "",
    },
  })
  const showToast = useCustomToast()
  const navigate = useNavigate()

  const resetPassword = async (data: NewPassword) => {
    const token = new URLSearchParams(window.location.search).get("token")
    if (!token) return
    await LoginService.resetPassword({
      requestBody: {
        new_password: data.new_password,
        token: { access_token: token },
      },
    })
  }

  const mutation = useMutation({
    mutationFn: resetPassword,
    onSuccess: () => {
      showToast("Success!", "Password updated successfully.", "success")
      reset()
      navigate({ to: "/login" })
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
  })

  const onSubmit: SubmitHandler<NewPasswordForm> = async (data) => {
    mutation.mutate(data)
  }

  return (
    <Container
      as="form"
      onSubmit={handleSubmit(onSubmit)}
      h="100vh"
      maxW="sm"
      alignItems="stretch"
      justifyContent="center"
      gap={4}
      centerContent
    >
      <Heading size="xl" color="ui.main" textAlign="center" mb={2}>
        Reset Password
      </Heading>
      <Text textAlign="center">
        Please enter your new password and confirm it to reset your password.
      </Text>

      <Field.Root invalid={!!errors.new_password} mt={4}>
        <Field.Label>Set Password</Field.Label>
        <Input
          id="password"
          {...register("new_password", passwordRules())}
          placeholder="Password"
          type="password"
        />
        {errors.new_password && (
          <Field.ErrorText>{errors.new_password.message}</Field.ErrorText>
        )}
      </Field.Root>

      <Field.Root invalid={!!errors.confirm_password} mt={4}>
        <Field.Label>Confirm Password</Field.Label>
        <Input
          id="confirm_password"
          {...register("confirm_password", confirmPasswordRules(getValues))}
          placeholder="Password"
          type="password"
        />
        {errors.confirm_password && (
          <Field.ErrorText>{errors.confirm_password.message}</Field.ErrorText>
        )}
      </Field.Root>

      <Button variant="outline" type="submit">
        Reset Password
      </Button>
    </Container>
  )
}

export default ResetPassword
