import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import {Footer} from "@/components/Common/Footer"
import { Flex, Box } from "@chakra-ui/react"
import { Outlet, createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout")({
    component: Layout,
})

function Layout() {

    return (
        <Box backgroundColor="background" minHeight="100vh">
        <Flex direction="column" justifyContent='space-between' backgroundColor="background">
            <SegmentedNavigation />
                <Outlet />
            <Footer/>
        </Flex>
        </Box>
  )
}
