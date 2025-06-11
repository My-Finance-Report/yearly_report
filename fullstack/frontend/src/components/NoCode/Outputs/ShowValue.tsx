import {
  FormatNumber,
  Card,
  Box,
  HStack,
  Stat,
  Text,
  Badge,
  VStack,
  Separator,
} from "@chakra-ui/react";
import { Chart, useChart } from "@chakra-ui/charts";
import { Area, AreaChart } from "recharts";
import { LuDollarSign, LuPercent } from "react-icons/lu";
import { ShowProps } from "./ShowTypes";
import { ResultWithTrend } from "@/client";

function mapUnitToSymbol(unit: string): React.ReactNode {
  switch (unit) {
    case "dollar":
      return <LuDollarSign />;
    case "percent":
      return <LuPercent />;
    default:
      return null;
  }
}

export function ShowValue({ widget }: ShowProps) {
  const result = widget.result as number;

  return (
    <Box borderWidth={1} borderRadius="md" p={2} minWidth={"300px"}>
      <Stat.Root
        border={"1px"}
        borderRadius={"md"}
        size="lg"
        flex={"row"}
        justifyContent={"space-between"}
        alignItems={"center"}
      >
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

export function ShowValueWithTrend({ widget }: ShowProps) {
  const result = widget.result as ResultWithTrend;
  const isUp = Number(result.trend) >= 0;
  return (
    <Box borderWidth={1} borderRadius="md" p={2} minWidth={"300px"}>
      <Stat.Root
        border={"1px"}
        borderRadius={"md"}
        size="lg"
        flex={"row"}
        justifyContent={"space-between"}
        alignItems={"center"}
      >
        <VStack>
          <Stat.ValueText>
            {result.result ? (
              <FormatNumber
                value={Number(result.result)}
                style="currency"
                currency="USD"
              />
            ) : (
              "N/A"
            )}
          </Stat.ValueText>
          <Badge
            colorPalette={isUp ? "green" : "red"}
            variant="plain"
            p="2"
            m="2"
          >
            {isUp ? <Stat.UpIndicator /> : <Stat.DownIndicator />}
            {result.trend}%
          </Badge>
        </VStack>
        <Stat.Label fontSize={"xl"}>{widget.name} </Stat.Label>
      </Stat.Root>
    </Box>
  );
}
export function ShowSeparator({ widget }: ShowProps) {
  return (
    <HStack>
      <Separator flex="1" />
      <Text flexShrink="0">{widget.name}</Text>
      <Separator flex="1" />
    </HStack>
  );
}

export function ShowBadge({ widget }: ShowProps) {
  return (
    <Badge p={2} m={2} variant="outline" colorPalette="orange">
      {widget.result as string}
    </Badge>
  );
}

export function ShowCardWithSparkline({ widget }: ShowProps) {
  const result = widget.result as ResultWithTrend;
  if (!result.result) {
    return (
      <Card.Root minW="250px" minH="200px" size="lg" overflow="hidden">
        <Card.Body>
          <Stat.Root>
            <Stat.Label>
              {mapUnitToSymbol(widget.result_type)} {widget.name}
            </Stat.Label>
            <Stat.ValueText>N/A</Stat.ValueText>
          </Stat.Root>
        </Card.Body>
        <Card.Footer>
          <Text fontSize={"xs"}>Must be plaid connected to see balances</Text>
        </Card.Footer>
      </Card.Root>
    );
  }

  return (
    <Card.Root minW="250px" minH="200px" size="lg" overflow="hidden">
      <Card.Body>
        <Stat.Root>
          <Stat.Label>
            {mapUnitToSymbol(result.unit)} {widget.name}
          </Stat.Label>
          <Stat.ValueText>
            <FormatNumber
              value={Number(result.result)}
              style="currency"
              currency="USD"
            />
          </Stat.ValueText>
        </Stat.Root>
      </Card.Body>
      <SparkLine data={result.trend_data} />
    </Card.Root>
  );
}

const SparkLine = ({
  data,
}: {
  data: { values: { value: string }[]; color: string };
}) => {
  const chart = useChart({
    data: data.values,
    series: [{ color: data.color }],
  });

  return (
    <Box w="300px" h="100px" overflow="hidden">
      <Chart.Root chart={chart}>
        <AreaChart
          width={300}
          height={100}
          data={chart.data}
          margin={{ top: 0, right: 0, left: 0, bottom: 0 }}
        >
          {chart.series.map((item) => (
            <Area
              key={item.name}
              isAnimationActive={false} // smoother at this size
              dataKey={chart.key(item.name)}
              fill={chart.color(item.color)}
              fillOpacity={0.2}
              stroke={chart.color(item.color)}
              strokeWidth={1} // thinner line looks cleaner
            />
          ))}
        </AreaChart>
      </Chart.Root>
    </Box>
  );
};
