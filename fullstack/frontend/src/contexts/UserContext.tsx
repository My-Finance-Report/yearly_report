import { createContext, useContext, useEffect, useState } from "react";
import { getCurrentUser } from "../hooks/useAuth";
import { UserOut } from "../client";

const UserContext = createContext<UserOut | null>(null);

export function UserProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<UserOut | null>(null);

  useEffect(() => {
    getCurrentUser().then(setUser);
  }, []);

  return <UserContext.Provider value={user}>{children}</UserContext.Provider>;
}

export function useUser() {
  return useContext(UserContext);
}
