import { createFileRoute } from "@tanstack/react-router"
import { WorkerStatus } from "@/components/Common/WorkerStatus"


export const Route = createFileRoute("/_layout/_logged_in/test")({
  component: Test,
})

function Test(){
    return (
        <WorkerStatus/>
    )
}