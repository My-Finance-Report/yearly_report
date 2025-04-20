import { NoCodeShow, renderNoCodeParameter } from "@/components/NoCode/Outputs/Show";
import {
  NoCodeWidgetOut,
  Parameter_Output,
} from "@/client";
import {
  Button,
  Grid,
  GridItem,
  Container,
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
  
export function NoCodeEditCanvas({ widgets, setEditWidget }: { widgets: NoCodeWidgetOut[]; setEditWidget: (widget: NoCodeWidgetOut) => void}) {
  
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

 
export function NoCodeDisplayCanvas({ widgets, parameters, setParameters }: { widgets: NoCodeWidgetOut[]; parameters: Parameter_Output[]; setParameters: (parameters: Parameter_Output[]) => void}) {

    if (!widgets) {
      return <div>No widgets found</div>
    }

    const updateAParameter = (parameter: Parameter_Output) => {
      setParameters(
        parameters.map((p) => (p.name === parameter.name ? parameter : p))
      );
    };


    const toDisplayParams = parameters.filter((parameter) =>  parameter.is_runtime && parameter.display_info)



    return (
      <Container>
      <Grid
        templateRows={`repeat(36, 1fr)`}
        templateColumns={`repeat(12, 1fr)`}
        gap={4}
      >
        {widgets.map(widget => (
          <GridItem

            key={widget.id || widget.name}
            rowStart={widget.row}
            colStart={widget.col}
            rowSpan={widget.row_span}
            colSpan={widget.col_span}
          >
            <NoCodeShow widget={widget} />
          </GridItem>
        ))}
        {toDisplayParams.map(param =>(
          <GridItem
            key={`${param.name}${param.widget_id}`}
            rowStart={param.display_info!.row} // we asserted above that its defined
            colStart={param.display_info!.col}
            rowSpan={param.display_info!.row_span}
            colSpan={param.display_info!.col_span}
          >
            {
          renderNoCodeParameter(param, updateAParameter)
            }
          </GridItem>
        ))}

      </Grid>
      </Container>
    )
  }
  
   