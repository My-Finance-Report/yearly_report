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
import React from "react";
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
  
 export function NoCodeDisplayCanvas({ widgets }: { widgets: NoCodeWidgetOut[]}) {
  
    if (!widgets) {
      return <div>No widgets found</div>
    }
  
    return (
      <Container>
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
  
   