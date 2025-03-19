import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import {Footer} from "@/components/Common/Footer"
import { Flex, Spinner } from "@chakra-ui/react"
import { Outlet, createFileRoute, redirect } from "@tanstack/react-router"
import useAuth, { isLoggedIn } from "../hooks/useAuth"

export const Route = createFileRoute("/_layout")({
    component: Layout,
    beforeLoad: async () => {
        if (!isLoggedIn()) {
            throw redirect({
                to: "/demo",
            })
        }
    },
})

function Layout() {
    const { isLoading, error,logout } = useAuth()

    if (error){
        logout()
    }

    return (
        <div style={{ backgroundColor: "background", minHeight: "100vh" }}>
            <SegmentedNavigation />
            {isLoading ? (
                <Flex justify="center" align="center" height="100vh" width="full">
                    <Spinner size="xl" color="ui.main" />
                </Flex>
            ) : (
                <Flex justify="center" align="center" width="full">
                    <Outlet />
                </Flex>
            )}
      <Footer/>
    </div>
  )
}
