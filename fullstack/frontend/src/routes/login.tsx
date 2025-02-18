import { Button, Container, Input, Field, Link, Text } from "@chakra-ui/react";
import {PasswordInput} from '@/components/ui/password-input'
import { createFileRoute, redirect, Link as RouterLink } from "@tanstack/react-router";
import { useForm, SubmitHandler } from "react-hook-form";
import { Body_login_login_access_token as AccessToken } from "../client";
import useAuth, { isLoggedIn } from "../hooks/useAuth";
import { emailPattern } from "../utils";

export const Route = createFileRoute("/login")({
  component: Login,
  beforeLoad: async () => {
    if (isLoggedIn()) {
      throw redirect({ to: "/" });
    }
  },
});

function Login() {
  const { loginMutation, error, resetError } = useAuth();
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
  });

  const onSubmit: SubmitHandler<AccessToken> = async (data) => {
    if (isSubmitting) return;
    resetError();
    try {
      await loginMutation.mutateAsync(data);
    } catch {
      // error is handled by useAuth hook
    }
  };

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
      <Field.Root>
        <Field.Label>Email</Field.Label>
        <Input
          id="username"
          {...register("username", {
            required: "Username is required",
            pattern: emailPattern,
          })}
          placeholder="Email"
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
          required
        />
        {error && <Field.ErrorText>{error}</Field.ErrorText>}
      </Field.Root>

      <Link as={RouterLink} href="/recover-password" color="blue.500">
        Forgot password?
      </Link>

      <Button variant="primary" type="submit" isLoading={isSubmitting}>
        Log In
      </Button>

      <Text>
        Don't have an account?{" "}
        <Link as={RouterLink} href="/signup" color="blue.500">
          Sign up
        </Link>
      </Text>
    </Container>
  );
}

export default Login;
