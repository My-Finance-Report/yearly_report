import { Outlet, createFileRoute, redirect } from "@tanstack/react-router";
import { getCurrentUser } from "../../hooks/useAuth";

export const Route = createFileRoute("/_layout/_logged_in")({
  component: Layout,
  beforeLoad: async () => {
    const user = await getCurrentUser();
    if (!user) {
      throw redirect({
        to: "/login",
      });
    }
  },
});

function Layout() {
  return <Outlet />;
}
