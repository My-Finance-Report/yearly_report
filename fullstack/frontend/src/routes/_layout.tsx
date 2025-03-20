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
                to: "/landing",
            })
        }
    },
})

function Layout() {
    const { isLoading } = useAuth()


    return (
            <Flex direction="column" justifyContent={'space-between'} minH="100vh" backgroundColor="background">
            <SegmentedNavigation />
            {isLoading ? (
                <Flex justify="center" align="center" height="100vh" width="full" mb={20}>
                    <Spinner size="xl" color="ui.main" />
                </Flex>
            ) : (
                    <Flex justify="center" align="center" width="full">
                        <Outlet />
                    </Flex>
                )}

            <Footer/>
        </Flex>
  )
}
