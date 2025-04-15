import { NoCodeWidget } from "@/client";
import { GenericPieChart } from "@/components/Charting/PieChart";
import { Box } from "@chakra-ui/react";

export function ShowChart({ widget }: { widget: NoCodeWidget }) {
  const result = widget.result as { key: string; value: number }[];
  console.log(result);
  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2}>
      <GenericPieChart data={result} dataKey={"value"} nameKey={"key"} />
    </Box>
  );
}
