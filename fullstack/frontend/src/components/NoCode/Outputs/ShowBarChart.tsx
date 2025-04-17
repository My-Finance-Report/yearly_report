import { NoCodeWidgetOut } from "@/client";
import { Box, Heading } from "@chakra-ui/react";
import { GenericBarChart } from "@/components/Charting/BarChart"

export function ShowBarChart({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as { key: string; value: number }[];

  const keysInAll = result.length === 0
    ? []
    : Object.keys(result[0]).filter(key =>
        result.every(obj => key in obj)
      );
  const nameKey = keysInAll[0]

  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2} minWidth={widget.width}>
        <Heading>{widget.name}</Heading>
      <GenericBarChart data={result} nameKey={nameKey} />
    </Box>
  );
}
