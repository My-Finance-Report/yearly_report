import { NoCodeWidgetOut, Parameter_Output} from "@/client"
import { NoCodeParameter } from "@/components/NoCode/Generators/Parameter";
import { ShowValue, ShowValueWithTrend, ShowBadge } from "./ShowValue"
import { ShowList } from "./ShowList"
import { ShowPieChart } from "./ShowPieChart"
import { ShowBarChart } from "./ShowBarChart"
import { Box, Flex } from "@chakra-ui/react"

const MAP_TO_SHOW = {
    "value": ShowValue,
    "value_with_trend": ShowValueWithTrend,
    "list": ShowList,    
    "pie_chart": ShowPieChart,
    "bar_chart": ShowBarChart,
    "badge": ShowBadge
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
    default:
      return null;
  }
}

function WidgetSpecificParmeters({
  runtime_parameters,
  setParameters,
}: {
  runtime_parameters: Parameter_Output[];
  setParameters: (parameters: Parameter_Output[]) => void;
}) {
  const updateAParameter = (parameter: Parameter_Output) => {
    setParameters(
      runtime_parameters.map((p) => (p.name === parameter.name ? parameter : p))
    );
  };
  const filteredParams = runtime_parameters.filter((parameter) => parameter.widget_id && parameter.is_runtime)


  return (
    <Flex direction="row" gap={0}>
      {filteredParams.map((parameter) =>
        renderNoCodeParameter(parameter, updateAParameter)
      )}
    </Flex>
  );
}




export function NoCodeShow({ widget, setRuntimeParameters }: { widget: NoCodeWidgetOut, setRuntimeParameters: (parameters: Parameter_Output[]) => void }) {
    const TheDisplay = MAP_TO_SHOW[widget.type]

    if (widget.result_type === "deferred") {
        return <div>Deferred</div>
    }

    console.log(widget)
    return (
        <Box>
            <WidgetSpecificParmeters runtime_parameters={widget.parameters} setParameters={setRuntimeParameters} />
            <TheDisplay widget={widget} />
        </Box>
    )
}
    