import { NoCodeWidgetOut } from "@/client";
import { Box, Heading } from "@chakra-ui/react";
import { GenericBarChart } from "@/components/Charting/BarChart"

export function ShowBarChart({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as { key: string; value: number }[];
  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2}>
        <Heading>{widget.name}</Heading>
      <GenericBarChart data={result} dataKey={"value"} nameKey={"key"} />
    </Box>
  );
}
