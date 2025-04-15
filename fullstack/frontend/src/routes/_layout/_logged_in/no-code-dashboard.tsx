import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { NoCodeShow } from "@/components/NoCode/Outputs/Show"
import { NoCodeService, NoCodeWidget } from "@/client"
import { Container, Flex, Heading } from "@chakra-ui/react"

export const Route = createFileRoute("/_layout/_logged_in/no-code-dashboard")({
  component: NoCodeDashboard,
})

function orderWidgets(widgets: NoCodeWidget[]): Array<Array<NoCodeWidget>> {
  const rows: Array<Array<NoCodeWidget>> = []
  for (const widget of widgets) {
    if (!rows[widget.row]) {
      rows[widget.row] = [widget]
    } else {
      rows[widget.row].push(widget)
    }
  }
  return rows.map(row=>row.sort((a, b) => a.col - b.col))
}

function NoCodeDashboard() {
  const { data: widgets, isLoading } = useQuery({
    queryKey: ["no-code-widgets"],
    queryFn: () => NoCodeService.getNoCodeDashboard(),
  });

  if (isLoading) {
    return <div>Loading...</div>
  }

  if (!widgets) {
    return <div>No widgets found</div>
  }

  return (
    <Container maxW="lg" my={8}>
      <Heading mb={4}>No-Code Dashboard</Heading>
      {orderWidgets(widgets).map((row) => (
        <Flex key={row[0].row} direction="row" gap={2}>
          {row.map((widget) => (
            <NoCodeShow key={widget.name} widget={widget} />
          ))}
        </Flex>
      ))}
    </Container>
  )
}
