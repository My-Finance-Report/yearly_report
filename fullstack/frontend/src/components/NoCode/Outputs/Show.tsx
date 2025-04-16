import { NoCodeWidgetOut } from "@/client"
import { ShowValue } from "./ShowValue"
import { ShowList } from "./ShowList"
import { ShowChart } from "./ShowChart"

const MAP_TO_SHOW = {
    "value": ShowValue,
    "list": ShowList,    
    "pie_chart": ShowChart    
}

export function NoCodeShow({ widget }: { widget: NoCodeWidgetOut }) {
    const TheDisplay = MAP_TO_SHOW[widget.type]
    return <TheDisplay widget={widget} />
}
    