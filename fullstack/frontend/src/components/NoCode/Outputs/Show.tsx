import { NoCodeWidgetOut} from "@/client"
import { ShowValue, ShowValueWithTrend, ShowBadge } from "./ShowValue"
import { ShowList } from "./ShowList"
import { ShowPieChart } from "./ShowPieChart"
import { ShowBarChart } from "./ShowBarChart"
import { Box } from "@chakra-ui/react"

const MAP_TO_SHOW = {
    "value": ShowValue,
    "value_with_trend": ShowValueWithTrend,
    "list": ShowList,    
    "pie_chart": ShowPieChart,
    "bar_chart": ShowBarChart,
    "badge": ShowBadge
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
    