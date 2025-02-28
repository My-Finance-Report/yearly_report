import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import { Flex, Spinner } from "@chakra-ui/react"
import { Outlet, createFileRoute, redirect } from "@tanstack/react-router"
import useAuth, { isLoggedIn } from "../hooks/useAuth"

export const Route = createFileRoute("/_layout")({
  component: Layout,
  beforeLoad: async () => {
    if (!isLoggedIn()) {
      throw redirect({
        to: "/login",
      })
    }
  },
})

function Layout() {
  const { isLoading } = useAuth()

  return (
    <div>
      <SegmentedNavigation />
      {isLoading ? (
        <Flex justify="center" align="center" height="100vh" width="full" >
          <Spinner size="xl" color="ui.main" />
        </Flex>
      ) : (

        <div style={{marginRight: 20}}>
        <Outlet />
        </div>
      )}
    </div>
  )
}
