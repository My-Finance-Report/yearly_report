import { PasswordInput } from "@/components/ui/password-input"
import { Button, Container, Field, Input, Link, Text } from "@chakra-ui/react"
import {
  Link as RouterLink,
  createFileRoute,
  redirect,
} from "@tanstack/react-router"
import { type SubmitHandler, useForm } from "react-hook-form"
import type { Body_login_login_access_token as AccessToken } from "@/client"
import useAuth, { isLoggedIn } from "@/hooks/useAuth"
import { emailPattern } from "../../utils"
import { useState } from "react"

export const Route = createFileRoute("/_layout/login")({
  component: Login,
  beforeLoad: async () => {
    if (isLoggedIn()) {
      throw redirect({ to: "/transactions" })
    }
  },
})

function Login() {
  const { loginMutation, error, resetError } = useAuth()
  const [blahError, setError] = useState(false)
  const {
    register,
    handleSubmit,
    formState: { errors, isSubmitting },
  } = useForm<AccessToken>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      username: "",
      password: "",
    },
  })

  const onSubmit: SubmitHandler<AccessToken> = async (data) => {
    if (isSubmitting) return
    resetError()
    try {
      await loginMutation.mutateAsync(data)
    } catch {
      setError(true)
    }
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
      {blahError && (
          <Text>Incorrect Username or Password</Text>
        )}
      <Field.Root>
        <Field.Label>Email</Field.Label>
        <Input
          id="username"
          {...register("username", {
            required: "Username is required",
            pattern: emailPattern,
          })}
          placeholder="Email"
          autoComplete="username"
          type="email"
          required
        />

        {errors.username && (
          <Field.ErrorText>{errors.username.message}</Field.ErrorText>
        )}
      </Field.Root>

      <Field.Root>
        <Field.Label>Password</Field.Label>
        <PasswordInput
          {...register("password", {
            required: "Password is required",
          })}
          placeholder="Password"
          autoComplete="current-password"
          required
        />
        {error && <Field.ErrorText>{error}</Field.ErrorText>}
      </Field.Root>
      <RouterLink to="/recover-password">
        <Link color="blue.500">Forgot password?</Link>
      </RouterLink>

      <Button variant="outline" type="submit" loading={isSubmitting}>
        Log In
      </Button>

      <Text>
        Don't have an account?{" "}
        <RouterLink to="/signup">
          <Link color="blue.500">Sign up</Link>
        </RouterLink>
      </Text>
    </Container>
  )
}

export default Login
