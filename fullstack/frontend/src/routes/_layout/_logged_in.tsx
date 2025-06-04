import { Outlet, createFileRoute, useRouter } from "@tanstack/react-router";
import { useUser } from "@/contexts/UserContext";
import { useEffect } from "react";
import { Spinner } from "@chakra-ui/react";



export const Route = createFileRoute("/_layout/_logged_in")({
  component: Layout,
});

function Layout() {
  const user = useUser();
  const router = useRouter();

  useEffect(() => {
    if (user === null) {
      router.navigate({ to: "/login" });
    }
  }, [user]);

  if (user === null) return <Spinner />;
  return <Outlet />;

}
