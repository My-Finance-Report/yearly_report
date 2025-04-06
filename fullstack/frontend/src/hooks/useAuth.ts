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

const isSessionValid = async (): Promise<boolean> => {
  try {
    await UsersService.readUserMe();
    return true;
  } catch {
    return false;
  }
}

const isSessionActive = (): boolean => {
  return sessionStorage.getItem("session_active") === "true"
}

const useAuth = () => {
  const [loginError, setLoginError] = useState<string | null>(null)
  const [requires2FA, setRequires2FA] = useState(false)
  const [requires2FASetup, setRequires2FASetup] = useState(false)
  const [tempToken, setTempToken] = useState<string | null>(null)
  const navigate = useNavigate()
  const showToast = useCustomToast()
  const queryClient = useQueryClient()
  
  const { data: user, isLoading, error: authError } = useQuery<UserOut | null, Error>({
    queryKey: ["currentUser"],
    queryFn: UsersService.readUserMe,
    enabled: isSessionActive(),
    retry: false,
  })

  if (authError) {
    // Clear the session indicator
    sessionStorage.removeItem("session_active")
    queryClient.clear()
    navigate({ to: "/login" })
  }

  const signUpMutation = useMutation({
    mutationFn: (data: UserRegister) =>
      UsersService.registerUser({ requestBody: data }),

    onSuccess: async (_, variables) => {
      try {
        const { email, password } = variables;
        
        const {  tempToken } = await login({
          username: email,
          password: password,
        });
        
        showToast(
          "Account created",
          "Your account has been created and you've been logged in successfully.",
          "success",
        );
        console.log("redirecting to setup")
        navigate({ to: "/setup_two_fa" , search: { tempToken: tempToken } });
      } catch (error: unknown) {
        console.log(error)
     
      }
    },
    onError: (err: ApiError) => {
      let errDetail = (err.body as { detail: string })?.detail;

      if (err instanceof AxiosError) {
        errDetail = err.message;
      }

      showToast("Something went wrong.", errDetail, "error");
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["users"] });
    },
  });

  interface LoginResult {
    success: boolean;
    requires2FA?: boolean;
    requires2FASetup?: boolean;
    tempToken?: string;
    error?: unknown;
  }

  const login = async (data: AccessToken): Promise<LoginResult> => {
    try {
      const response = await LoginService.loginAccessToken({
        formData: data,
      });

      if (response.requires_2fa_setup) {
        if (!response.temp_token) {
          throw new Error("Temp token not found");
        }
        setRequires2FASetup(true);
        setTempToken(response.temp_token);
        return { success: false, requires2FA: false, requires2FASetup: true, tempToken: response.temp_token };
      }

      if (response.requires_2fa) {
        if (!response.temp_token) {
          throw new Error("Temp token not found");
        }
        setRequires2FA(true);
        setTempToken(response.temp_token);
        return { success: false, requires2FA: true, requires2FASetup: false, tempToken: response.temp_token };
      }
      
      // With HttpOnly cookies, we don't need to manually store the token
      sessionStorage.setItem("session_active", "true");
      
      return { success: true };
    } catch (error: unknown) {
      console.error("Login error:", error);
      return { success: false, error };
    }
  };

  const loginMutation = useMutation({
    mutationFn: login,
    onSuccess: (result) => {
      if (result.success) {
        console.log("redirecting here")
        navigate({ to: "/" });
      }
    },
    onError: (err: unknown) => {
      // Don't show error for 2FA cases, as they're handled separately
      if (typeof err === 'object' && err !== null && 'requires2FA' in err) {
        return;
      }
      
      let errDetail = "Something went wrong";
      
      if (err instanceof AxiosError) {
        errDetail = err.message;
      } else if (typeof err === 'object' && err !== null && 'error' in err && err.error) {
        errDetail = String(err.error);
      }

      if (Array.isArray(errDetail)) {
        errDetail = "Something went wrong";
      }

      setLoginError(errDetail);
    },
  });

  // Custom function to call the logout endpoint
  const callLogoutEndpoint = async () => {
    try {
      // Call the logout endpoint using fetch directly
      await LoginService.logout();
      return true;
    } catch {
      console.error("Logout API error:");
      return false;
    }
  };

  const logout = async () => {
    try {
      await callLogoutEndpoint();
    } catch {
      console.error("Logout error:");
    } finally {
      sessionStorage.removeItem("session_active");
      queryClient.clear();
      navigate({ to: "/login" });
    }
  };

  const reset2FAStates = () => {
    setRequires2FA(false);
    setRequires2FASetup(false);
    setTempToken(null);
  };

  return {
    signUpMutation,
    loginMutation,
    logout,
    user,
    isLoading,
    error: loginError,
    requires2FA,
    requires2FASetup,
    tempToken,
    resetError: () => setLoginError(null),
    reset2FAStates,
  };
};

export { isSessionActive, isSessionValid };
export default useAuth;
