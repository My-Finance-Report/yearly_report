import { GenericPieChart } from "@/components/Charting/PieChart";
import { Card, Stat } from "@chakra-ui/react";
import { ShowProps } from "./ShowTypes";
import { KeyValuePair } from "@/client";

export function ShowPieChart({ widget }: ShowProps) {
  const result = widget.result as KeyValuePair[];
  const optimisticallyParsedData = result.map((item) => ({
    ...item,
    value: Number(item.value ?? 0),
  }));

  return (
    <Card.Root maxH="350px" minW="250px" size="lg" overflow="hidden">
      <Card.Body>
        <Stat.Root>
          <Stat.Label>{widget.name}</Stat.Label>
        </Stat.Root>
      </Card.Body>
      <GenericPieChart
        data={optimisticallyParsedData}
        dataKey={"value"}
        nameKey={"key"}
      />
    </Card.Root>
  );
}
