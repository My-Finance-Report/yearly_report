import { NoCodeShow } from "@/components/NoCode/Outputs/Show";
import {
  NoCodeWidgetOut,
  Parameter_Output,
} from "@/client";
import {
  Container,
  Button,
  Box,
  Flex,
} from "@chakra-ui/react";
import { BsPencilSquare } from "react-icons/bs";
import { NoCodeParameter } from "./Generators/Parameter";


function orderWidgets(widgets: NoCodeWidgetOut[]): Array<Array<NoCodeWidgetOut>> {
    const rows: Array<Array<NoCodeWidgetOut>> = []
    for (const widget of widgets) {
      if (!rows[widget.row]) {
        rows[widget.row] = [widget]
      } else {
        rows[widget.row].push(widget)
      }
    }
    return rows.map(row => row.sort((a, b) => a.col - b.col))
  }
  
export function NoCodeEditCanvas({ widgets, setEditWidget }: { widgets: NoCodeWidgetOut[]; setEditWidget: (widget: NoCodeWidgetOut) => void }) {
  
    if (!widgets) {
      return <div>No widgets found</div>
    }
  
    return (
      <Container>
        {orderWidgets(widgets).map((row) => (
          <Flex key={row[0].row} direction="row" gap={2}>
            {row.map((widget) => (
              <Box>
                <Button onClick={() => setEditWidget(widget)}>
                  <BsPencilSquare />
                </Button>
                <NoCodeShow key={widget.name} widget={widget} />
              </Box>
            ))}
          </Flex>
        ))}
      </Container>
    )
  }
function renderNoCodeParameter(
  parameter: Parameter_Output,
  updateAParameter: (parameter: Parameter_Output) => void
) {
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

export function RuntimeParameters({
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

  return (
    <Flex direction="row" gap={0}>
      {runtime_parameters.map((parameter) =>
        renderNoCodeParameter(parameter, updateAParameter)
      )}
    </Flex>
  );
}

 
export function NoCodeDisplayCanvas({ widgets, runtimeParameters, setParameters }: { widgets: NoCodeWidgetOut[]; runtimeParameters: Parameter_Output[]; setParameters: (parameters: Parameter_Output[]) => void}) {

    if (!widgets) {
      return <div>No widgets found</div>
    }

    return (
      <Container>
        <RuntimeParameters runtime_parameters={runtimeParameters} setParameters={setParameters} />
        {orderWidgets(widgets).map((row) => (
          <Flex key={row[0].row} direction="row" gap={2}>
            {row.map((widget) => (
                <NoCodeShow key={widget.name} widget={widget} />
            ))}
          </Flex>
        ))}
      </Container>
    )
  }
  
   