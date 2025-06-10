import { GenericPieChart } from "@/components/Charting/PieChart";
import { Card, Stat } from "@chakra-ui/react";
import { ShowProps } from "./ShowTypes";

export function ShowPieChart({ widget }: ShowProps) {
  const result = widget.result as { key: string; value: number }[];
  return (
    <Card.Root maxH="350px" minW="250px" size="lg" overflow="hidden">
      <Card.Body>
        <Stat.Root>
          <Stat.Label>{widget.name}</Stat.Label>
        </Stat.Root>
      </Card.Body>
      <GenericPieChart data={result} dataKey={"value"} nameKey={"key"} />
    </Card.Root>
  );
}
