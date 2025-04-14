import {
  CollapsibleWorkerStatus,
  WorkerStatus,
} from "@/components/Common/WorkerStatus"
import { Flex } from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/_logged_in/test")({
  component: Test,
})

function Test() {
  return (
    <Flex direction="row">
      <WorkerStatus />
      <CollapsibleWorkerStatus />
    </Flex>
  )
}
