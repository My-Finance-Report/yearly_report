import { PipelineEnd } from "@/client"
import { Text } from "@chakra-ui/react"

export function ShowValue( {pipelineEnd}: { pipelineEnd: PipelineEnd }) {
    return <Text>{pipelineEnd.result.value}</Text>
}
