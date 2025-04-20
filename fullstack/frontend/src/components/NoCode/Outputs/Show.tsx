import { NoCodeWidgetOut, Parameter_Output} from "@/client"
import { NoCodeParameter } from "@/components/NoCode/Generators/Parameter";
import { ShowValue, ShowBadge, ShowValueWithTrend, ShowSeparator } from "./ShowValue"
import { ShowList } from "./ShowList"
import { ShowPieChart } from "./ShowPieChart"
import { ShowBarChart } from "./ShowBarChart"

const MAP_TO_SHOW = {
    "value": ShowValue,
    "value_with_trend": ShowValueWithTrend,
    "list": ShowList,    
    "pie_chart": ShowPieChart,
    "bar_chart": ShowBarChart,
    "badge": ShowBadge,
    "separator" : ShowSeparator
}

export function renderNoCodeParameter(
  parameter: Parameter_Output,
  updateAParameter: (parameter: Parameter_Output) => void
) {

    console.log(parameter.type)
  switch (parameter.type) {
    case "int":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "int" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
      );
    case "float":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "float" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
      );
    case "string":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "string" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
      );
    case "select":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "select" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
      );
    case "multi_select":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "multi_select" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
      );
    case 'pagination':
        return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "pagination" }>}
          onChange={(value) => updateAParameter({ ...parameter, value })}
        />
        )
    default:
        throw new Error("unknown param")
  }
}



export function NoCodeShow({ widget }: { widget: NoCodeWidgetOut}) {
    const TheDisplay = MAP_TO_SHOW[widget.type]


    if (widget.result_type === "deferred") {
        return <div>Deferred</div>
    }

    return (
            <TheDisplay widget={widget}/>
    )
}
    