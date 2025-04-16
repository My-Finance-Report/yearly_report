import { NoCodeWidgetOut } from "@/client";
import { FormatNumber, Box, HStack, Stat, Text } from "@chakra-ui/react";

export function ShowValue({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as number;
  return (
    <Box borderWidth={1} borderRadius="md" p={2} m={2}>
      <Stat.Root border={"1px"} borderRadius={"md"}>
        <Stat.Label>{widget.name} </Stat.Label>
        <HStack>
          <Stat.ValueText>
            {widget.result_type === "number" ? (
              <FormatNumber value={result} style="currency" currency="USD" />
            ) : (
              <Text>{result}</Text>
            )}
          </Stat.ValueText>
        </HStack>
      </Stat.Root>
    </Box>
  );
}
