import { NoCodeWidgetOut} from "@/client"
import { ShowValue } from "./ShowValue"
import { ShowList } from "./ShowList"
import { ShowPieChart } from "./ShowPieChart"
import { ShowBarChart } from "./ShowBarChart"
import { Box } from "@chakra-ui/react"

const MAP_TO_SHOW = {
    "value": ShowValue,
    "list": ShowList,    
    "pie_chart": ShowPieChart,
    "bar_chart": ShowBarChart    
}

export function NoCodeShow({ widget }: { widget: NoCodeWidgetOut }) {
    const TheDisplay = MAP_TO_SHOW[widget.type]

    if (widget.result_type === "deferred") {
        return <div>Deferred</div>
    }
    return (
        <Box>
            <TheDisplay widget={widget} />
        </Box>
    )
}
    