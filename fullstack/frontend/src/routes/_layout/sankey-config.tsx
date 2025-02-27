import { SankeyConfigPage } from "@/components/Common/SankeyConfig"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/sankey-config")({
  component: SankeyConfigPage,
})
