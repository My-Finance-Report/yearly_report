import { PipelineEnd } from "@/client"
import { ShowValue } from "./ShowValue"
import { ShowList } from "./ShowList"

const MAP_TO_SHOW = {
    "show_value": ShowValue,
    "show_list": ShowList    
}

export function NoCodeShow({ pipelineEnd }: { pipelineEnd: PipelineEnd }) {
    const TheDisplay = MAP_TO_SHOW[pipelineEnd.output_type]
    return <TheDisplay pipelineEnd={pipelineEnd} />
}
    