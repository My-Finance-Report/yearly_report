import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import {Footer} from "@/components/Common/Footer"
import { Flex } from "@chakra-ui/react"
import { Outlet, createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout")({
    component: Layout,
})

function Layout() {

    return (
        <Flex direction="column" justifyContent='space-between' minH="100vh" backgroundColor="background">
            <SegmentedNavigation />
                <Outlet />

            <Footer/>
        </Flex>
  )
}
