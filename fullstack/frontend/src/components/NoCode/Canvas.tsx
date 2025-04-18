import { NoCodeShow } from "@/components/NoCode/Outputs/Show";
import {
  NoCodeWidgetOut,
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



function RuntimeParameters({runtime_parameters, setParameters}: {runtime_parameters: Parameter[]; setParameters: (parameters: Parameter[]) => void}) {

    const updateAParameter = (parameter: Parameter) => {
        setParameters(runtime_parameters.map(p => p.name === parameter.name ? parameter : p))
    }


    return (
      runtime_parameters.map((parameter) => (
        <NoCodeParameter key={parameter.name} parameter={parameter} onChange={(value: string | number) => updateAParameter({ ...parameter, value })} />
      ))
    )
  }
  
export function NoCodeDisplayCanvas({ widgets, runtimeParameters, setParameters }: { widgets: NoCodeWidgetOut[]; runtimeParameters: Parameter[]; setParameters: (parameters: Parameter[]) => void}) {

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
  
   