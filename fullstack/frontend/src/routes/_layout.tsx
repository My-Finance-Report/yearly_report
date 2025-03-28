import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import {Footer} from "@/components/Common/Footer"
import { Flex, Spinner } from "@chakra-ui/react"
import { Outlet, createFileRoute } from "@tanstack/react-router"
import useAuth from "../hooks/useAuth"

export const Route = createFileRoute("/_layout")({
    component: Layout,
})

function Layout() {
    const { isLoading } = useAuth()

    return (
        <Flex direction="column" justifyContent='space-between' minH="100vh" backgroundColor="background">
            <SegmentedNavigation />
            {isLoading ? (
                <Flex justify="center" align="center" height="100vh" width="full" mb={20}>
                    <Spinner size="xl" color="ui.main" />
                </Flex>
            ) : (
                <Outlet />
            )}

            <Footer/>
        </Flex>
  )
}
