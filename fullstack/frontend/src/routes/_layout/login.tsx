import { PasswordInput } from "@/components/ui/password-input"
import { 
  Button, 
  Container, 
  Field, 
  Input, 
  Link, 
  Text, 
  Flex,
  Box
} from "@chakra-ui/react"
import {
  Link as RouterLink,
  createFileRoute,
  redirect,
  useNavigate,
} from "@tanstack/react-router"
import { type SubmitHandler, useForm } from "react-hook-form"
import useAuth, { isLoggedIn } from "@/hooks/useAuth"
import { emailPattern } from "../../utils"
import { useState } from "react"
import { FcGoogle } from "react-icons/fc"
import useCustomToast from "@/hooks/useCustomToast"
import { OauthService } from "@/client"
import TwoFactorVerification from "@/components/TwoFactorVerification"
import TwoFactorSetup from "@/components/TwoFactorSetup"
import { useQueryClient } from "@tanstack/react-query"

interface LoginFormData {
  username: string;
  password: string;
}

export const Route = createFileRoute("/_layout/login")({
  component: Login,
  beforeLoad: async () => {
    if (await isLoggedIn()) {
      throw redirect({ to: "/transactions" })
    }
  },
})

function Login() {
  const { 
    loginMutation, 
    error, 
    resetError, 
    requires2FA, 
    requires2FASetup, 
    tempToken,
    reset2FAStates 
  } = useAuth()

  const [blahError, setError] = useState(false)
  const showToast = useCustomToast()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const {
    register,
    handleSubmit,
    formState: { errors, isSubmitting },
  } = useForm<LoginFormData>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      username: "",
      password: "",
    },
  })

  const onSubmit: SubmitHandler<LoginFormData> = async (data) => {
    if (isSubmitting) return
    resetError()
    try {
      await loginMutation.mutateAsync(data)
    } catch {
      setError(true)
    }
  }

  const [isGoogleLoading, setIsGoogleLoading] = useState(false);
  const handleGoogleLogin = async () => {
    try {
      setIsGoogleLoading(true);
      const {url} = await OauthService.loginGoogle()
      
      if (url) {
        window.location.href = url;
      } else {
        showToast("Error", "Failed to initiate Google login", "error");
      }
    } catch (error) {
      console.error("Google login error:", error);
      showToast("Error", "Failed to connect to authentication service", "error");
    } finally {
      setIsGoogleLoading(false);
    }
  };

  const handleBack = () => {
    reset2FAStates();
  };

  const handleSuccessfulVerification = () => {
    // Set the session as active to enable user queries
    sessionStorage.setItem("session_active", "true")
    
    // Force a refresh of the current user data
    queryClient.invalidateQueries({ queryKey: ["currentUser"] })
    
    reset2FAStates();
    
    navigate({ to: "/" });
  };

  if (requires2FA) {
    if (!tempToken) {
      throw new Error("No temp token found");
    }
    return (
      <Container maxW="sm" py={8}>
        <TwoFactorVerification 
          onCancel={handleBack}
          onSuccess={handleSuccessfulVerification}
          temp_token={tempToken}
        />
      </Container>
    );
  }

  if (requires2FASetup) {
    if (!tempToken) {
      throw new Error("No temp token found");
    }
    return (
      <Container maxW="sm" py={8}>
        <TwoFactorSetup 
          onComplete={handleBack}
          tempToken={tempToken}
        />
      </Container>
    );
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

      <Button variant="outline" type="submit" disabled={isSubmitting}>
        Log In
      </Button>

      <Flex align="center" my={4}>
        <Box flex="1" h="1px" bg="gray.200" />
        <Text px={3} fontSize="sm" color="gray.500">OR</Text>
        <Box flex="1" h="1px" bg="gray.200" />
      </Flex>

      <Button 
        variant="outline" 
        onClick={handleGoogleLogin}
        disabled={isGoogleLoading}
      >
        <Flex align="center">
          <Box mr={2}><FcGoogle size={20} /></Box>
          <Text>Sign in with Google</Text>
        </Flex>
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
