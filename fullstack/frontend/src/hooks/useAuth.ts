import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { useNavigate } from "@tanstack/react-router"
import { useState } from "react"

import { AxiosError } from "axios"
import {
  type Body_login_login_access_token as AccessToken,
  type ApiError,
  LoginService,
  type UserOut,
  type UserRegister,
  UsersService,
} from "../client"
import useCustomToast from "./useCustomToast"

const isLoggedIn = () => {
  return localStorage.getItem("access_token") !== null
}

const useAuth = () => {
  const [error, setError] = useState<string | null>(null)
  const [requires2FA, setRequires2FA] = useState(false)
  const [requires2FASetup, setRequires2FASetup] = useState(false)
  const [tempToken, setTempToken] = useState<string | null>(null)
  const navigate = useNavigate()
  const showToast = useCustomToast()
  const queryClient = useQueryClient()
  const { data: user, isLoading, error: authError } = useQuery<UserOut | null, Error>({
    queryKey: ["currentUser"],
    queryFn: UsersService.readUserMe,
    enabled: isLoggedIn(),
  })

    if (authError){
        localStorage.removeItem("access_token")
        queryClient.clear()
        navigate({ to: "/login" })
    }

  const signUpMutation = useMutation({
    mutationFn: (data: UserRegister) =>
      UsersService.registerUser({ requestBody: data }),

    onSuccess: () => {
      navigate({ to: "/login" })
      showToast(
        "Account created.",
        "Your account has been created successfully.",
        "success",
      )
    },
    onError: (err: ApiError) => {
      let errDetail = (err.body as { detail: string })?.detail

      if (err instanceof AxiosError) {
        errDetail = err.message
      }

      showToast("Something went wrong.", errDetail, "error")
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["users"] })
    },
  })

  const login = async (data: AccessToken) => {
    try {
      const response = await LoginService.loginAccessToken({
        formData: data,
      })

      
      if (response.requires_2fa) {
        setRequires2FA(true)
        if (response.temp_token) {
          setTempToken(response.temp_token)
          throw { type: '2fa_required', token: response.temp_token }
        }
      }
      
      if (response.requires_2fa_setup) {
        setRequires2FASetup(true)
        setTempToken(response.temp_token)
        throw { type: '2fa_setup_required' }
      }
      
      if (response.access_token) {
        localStorage.setItem("access_token", response.access_token)
      }
      
      return response
    } catch (error: unknown) {
      if (
        error && 
        typeof error === 'object' && 
        'type' in error && 
        (error.type === '2fa_required' || error.type === '2fa_setup_required')
      ) {
        throw error
      }
      
      console.error("Login error:", error)
      throw error
    }
  }

  const loginMutation = useMutation({
    mutationFn: login,
    onSuccess: () => {
      navigate({ to: "/" })
    },
    onError: (err: Error | ApiError | { type: string; token?: string }) => {
      // Don't show error for 2FA cases, as they're handled separately
      if (typeof err === 'object' && 'type' in err && (err.type === '2fa_required' || err.type === '2fa_setup_required')) {
        return
      }
      
      let errDetail = "Something went wrong"
      
      if (err instanceof AxiosError) {
        errDetail = err.message
      } else if ((err as ApiError).body) {
        errDetail = ((err as ApiError).body as {detail: string})?.detail
      }

      if (Array.isArray(errDetail)) {
        errDetail = "Something went wrong"
      }

      setError(errDetail)
    },
  })

  const logout = () => {
    localStorage.removeItem("access_token")
    queryClient.clear()
    navigate({ to: "/login" })
  }

  const reset2FAStates = () => {
    setRequires2FA(false)
    setRequires2FASetup(false)
    setTempToken(null)
  }

  return {
    signUpMutation,
    loginMutation,
    logout,
    user,
    isLoading,
    error,
    requires2FA,
    requires2FASetup,
    tempToken,
    resetError: () => setError(null),
    reset2FAStates,
  }
}

export { isLoggedIn }
export default useAuth
