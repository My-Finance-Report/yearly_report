import { NoCodeWidgetOut } from "@/client";
import { GenericPieChart } from "@/components/Charting/PieChart";
import { Card, Stat } from "@chakra-ui/react";

export function ShowPieChart({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as { key: string; value: number }[];
  console.log(result);
  return (
    <Card.Root minW="250px" size="lg" overflow="hidden">
      <Card.Body>
        <Stat.Root>
          <Stat.Label>
            {widget.name}
          </Stat.Label>
        </Stat.Root>
      </Card.Body>
      <GenericPieChart data={result} dataKey={"value"} nameKey={"key"} />
    </Card.Root>



  );
}
