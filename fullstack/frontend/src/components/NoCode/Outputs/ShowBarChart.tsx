import { Box, Heading } from "@chakra-ui/react";
import { GenericBarChart } from "@/components/Charting/BarChart";
import { ShowProps } from "./ShowTypes";
import { NoCodeAggregateData } from "@/client";

export function ShowBarChart({ widget }: ShowProps) {
  const result = widget.result as NoCodeAggregateData[];

  const keysInAll =
    result.length === 0
      ? []
      : Object.keys(result[0]).filter((key) =>
          result.every((obj) => key in obj),
        );
  const nameKey = keysInAll[0];

  return (
    <Box borderWidth={1} borderRadius="md" p={2}>
      <Heading>{widget.name}</Heading>
      <GenericBarChart data={result} nameKey={nameKey} />
    </Box>
  );
}
