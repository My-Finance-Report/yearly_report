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

export function NoCodeEditCanvas({ widgets, parameters, setParameters, setEditWidget }: { widgets: NoCodeWidgetOut[]; setEditWidget: (widget: NoCodeWidgetOut) => void ; parameters: Parameter_Output[]; setParameters: (parameters: Parameter_Output[]) => void }) {

    const updateAParameter = (parameter: Parameter_Output) => {
        setParameters(
            parameters.map((p) => (p.name === parameter.name ? parameter : p))
        );
    };




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
                    <NoCodeShow key={widget.name} widget={widget} updateAParameter={updateAParameter} />
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

 
export function NoCodeDisplayCanvas({ widgets, parameters, setParameters, refetch }: { widgets: NoCodeWidgetOut[]; parameters: Parameter_Output[]; setParameters: (parameters: Parameter_Output[]) => void, refetch: ()=>void}) {

    console.log(parameters)

    if (!widgets) {
      return <div>No widgets found</div>
    }

    const updateAParameter = (parameter: Parameter_Output, shouldRefetch:boolean=true) => {
      setParameters(
        parameters.map((p) => (p.name === parameter.name ? parameter : p))
      );
      console.log('set the param to', parameter)
      if (shouldRefetch){
          console.log("refetching")
          refetch()
      }
    };


    const toDisplayParams = parameters.filter((parameter) =>  parameter.is_runtime && parameter.display_info)



    return (
      <Container w="100%">
      <Grid
        templateRows={`repeat(12, 40px)`}
        templateColumns={`repeat(12, 100px)`}
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
            <NoCodeShow widget={widget} updateAParameter={updateAParameter} />
          </GridItem>
        ))}
        {toDisplayParams.map(param =>(
          <GridItem
            key={`${param.name}${param.widget_id}`}
            rowStart={param.display_info!.row} 
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
  
   
