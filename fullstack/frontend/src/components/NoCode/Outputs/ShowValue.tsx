import { NoCodeWidgetOut } from "@/client";
import { FormatNumber, Box, HStack, Stat, Text, Badge, VStack } from "@chakra-ui/react";

export function ShowValue({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as number;

  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2} minWidth={"300px"}>
      <Stat.Root border={"1px"} borderRadius={"md"} size="lg" flex={"row"} justifyContent={"space-between"} alignItems={"center"}>
        <HStack>
          <Stat.ValueText>
            {widget.result_type === "number" ? (
              <FormatNumber value={result} style="currency" currency="USD" />
            ) : (
              <Text>{result}</Text>
            )}
          </Stat.ValueText>
        </HStack>
        <Stat.Label fontSize={"xl"}>{widget.name} </Stat.Label>
      </Stat.Root>
    </Box>
  );
}

export function ShowValueWithTrend({ widget }: { widget: NoCodeWidgetOut }) {
  const {result, trend} = widget.result;
  const isUp = trend > 0;
  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2} minWidth={"300px"}>
      <Stat.Root border={"1px"} borderRadius={"md"} size="lg" flex={"row"} justifyContent={"space-between"} alignItems={"center"}>
        <VStack>
          <Stat.ValueText>
            {widget.result_type === "number" ? (
              <FormatNumber value={result} style="currency" currency="USD" />
            ) : (
              <Text>{result}</Text>
            )}
          </Stat.ValueText>
          <Badge colorPalette={isUp ? "green" : "red"} variant="plain" p="2" m="2">
            {isUp ? <Stat.UpIndicator /> : <Stat.DownIndicator />}
            {isUp ? "" : "-"}{trend}%
          </Badge>
        </VStack>
        <Stat.Label fontSize={"xl"}>{widget.name} </Stat.Label>
      </Stat.Root>
    </Box>
  );
}

export function ShowBadge({ widget }: { widget: NoCodeWidgetOut }) {
  return (
    <Badge  p={2} m={2} variant="outline" colorPalette="orange">{widget.result as string}</Badge>
  );
}
