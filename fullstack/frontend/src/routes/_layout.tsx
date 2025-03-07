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
    <div style={{ backgroundColor: "background", padding: 20, minHeight: "100vh" }}>
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
    </div>
  )
}
