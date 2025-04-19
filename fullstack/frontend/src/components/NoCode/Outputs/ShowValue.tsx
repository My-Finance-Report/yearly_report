import { NoCodeWidgetOut } from "@/client";
import { FormatNumber,Card, Box, HStack, Stat, Text, Badge, VStack } from "@chakra-ui/react";
import { Chart, useChart } from "@chakra-ui/charts"
import { Area, AreaChart } from "recharts"
import { LuDollarSign, LuPercent } from "react-icons/lu";


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

export function ShowValue({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as number;

  return (
    <Box borderWidth={1} borderRadius="md" p={2}  minWidth={"300px"}>
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
  const result = widget.result as { result: number, trend: number };
  const isUp = result.trend > 0;
  return (
    <Box borderWidth={1} borderRadius="md" p={2} minWidth={"300px"}>
      <Stat.Root border={"1px"} borderRadius={"md"} size="lg" flex={"row"} justifyContent={"space-between"} alignItems={"center"}>
        <VStack>
          <Stat.ValueText>
            {widget.result_type === "number" ? (
              <FormatNumber value={result.result} style="currency" currency="USD" />
            ) : (
              <Text>{result.result}</Text>
            )}
          </Stat.ValueText>
          <Badge colorPalette={isUp ? "green" : "red"} variant="plain" p="2" m="2">
            {isUp ? <Stat.UpIndicator /> : <Stat.DownIndicator />}
            {result.trend}%
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


export function ShowCardWithSparkline({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as { result: number|null, unit: string, trend_data: { values: { value: number }[], color: string } };
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
  )
  }
   
 return (
    <Card.Root minW="250px" minH="200px" size="lg" overflow="hidden">
      <Card.Body>
        <Stat.Root>
          <Stat.Label>
            {mapUnitToSymbol(result.unit)} {widget.name}
          </Stat.Label>
          <Stat.ValueText>
              <FormatNumber value={result.result} style="currency" currency="USD" />
            </Stat.ValueText>
        </Stat.Root>
      </Card.Body>
      <SparkLine data={result.trend_data} />
    </Card.Root>
  )
}


const SparkLine = ({ data }: { data: { values: { value: number }[], color: string } }) => {
  const chart = useChart({
    data: data.values,
    series: [{ color: data.color }],
  })

  return (
    <Chart.Root height="10" chart={chart}>
      <AreaChart
        data={chart.data}
        margin={{ top: 0, right: 0, left: 0, bottom: 0 }}
      >
        {chart.series.map((item) => (
          <Area
            key={item.name}
            isAnimationActive={true}
            dataKey={chart.key(item.name)}
            fill={chart.color(item.color)}
            fillOpacity={0.2}
            stroke={chart.color(item.color)}
            strokeWidth={2}
          />
        ))}
      </AreaChart>
    </Chart.Root>
  )
}
