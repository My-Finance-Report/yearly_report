import { Outlet, createFileRoute, redirect } from "@tanstack/react-router"
import { isSessionValid } from "../../hooks/useAuth"

export const Route = createFileRoute("/_layout/_logged_in")({
    component: Layout,
    beforeLoad: async () => {
        if (!isSessionValid()) {
            throw redirect({
                to: "/",
            })
        }
    },
})

function Layout() {
    return (
        <Outlet />
  )
}

