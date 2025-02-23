import { createFileRoute } from "@tanstack/react-router"
import { SankeyConfigPage } from '@/components/Common/SankeyConfig'

export const Route = createFileRoute("/_layout/sankey-config")({
    component: SankeyConfigPage,
})

