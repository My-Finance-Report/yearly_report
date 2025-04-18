import { NoCodeShow, renderNoCodeParameter } from "@/components/NoCode/Outputs/Show";
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
  
export function NoCodeEditCanvas({ widgets, setEditWidget, setRuntimeParameters }: { widgets: NoCodeWidgetOut[]; setEditWidget: (widget: NoCodeWidgetOut) => void, setRuntimeParameters: (parameters: Parameter_Output[]) => void }) {
  
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
                <NoCodeShow key={widget.name} widget={widget} setRuntimeParameters={setRuntimeParameters} />
              </Box>
            ))}
          </Flex>
        ))}
      </Container>
    )
  }

export function GlobalParameters({
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
      {runtime_parameters.filter((parameter) => !parameter.widget_id && parameter.is_runtime).map((parameter) =>
        renderNoCodeParameter(parameter, updateAParameter)
      )}
    </Flex>
  );
}

 
export function NoCodeDisplayCanvas({ widgets, globalParameters, setParameters }: { widgets: NoCodeWidgetOut[]; globalParameters: Parameter_Output[]; setParameters: (parameters: Parameter_Output[]) => void}) {

    if (!widgets) {
      return <div>No widgets found</div>
    }

    return (
      <Container>
        <GlobalParameters runtime_parameters={globalParameters} setParameters={setParameters} />
        <Flex direction="column" gap={2}>
        {orderWidgets(widgets).map((row) => (
          <Flex key={row[0].row} direction="row" gap={2} alignItems={"end"}>
            {row.map((widget) => (
                <NoCodeShow key={widget.name} widget={widget} setRuntimeParameters={setParameters} />
            ))}
          </Flex>
        ))}
</Flex>
      </Container>
    )
  }
  
   