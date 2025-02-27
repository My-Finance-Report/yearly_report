import { Cell, Pie, PieChart, Sector } from "recharts"
import type { PieSectorDataItem } from "recharts/types/polar/Pie"

import { CardContent, CardFooter } from "@/components/ui/card"
import {
  type ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart"
import { Box } from "@chakra-ui/react"
import useColorPalette from "@/hooks/useColorPalette"

export interface GenericChartDataItem {
  [key: string]: string | number
}

export interface GenericPieChartProps {
  data: GenericChartDataItem[]
  dataKey: keyof GenericChartDataItem
  nameKey: keyof GenericChartDataItem
  config?: ChartConfig | null
  innerRadius?: number
  activeIndex?: number
  activeShape?: (props: PieSectorDataItem) => JSX.Element
}

export function GenericPieChart({
  data,
  dataKey,
  nameKey,
  config,
  innerRadius = 60,
  activeIndex = 0,
  activeShape = ({ outerRadius = 0, ...props }: PieSectorDataItem) => (
    <Sector {...props} outerRadius={outerRadius + 10} />
  ),
}: GenericPieChartProps) {

  const colorPalette = useColorPalette()

  let finalConfig: ChartConfig
  if (!config) {
    const uniqueNames = Array.from(new Set(data.map((item) => item[nameKey])))
    finalConfig = uniqueNames.reduce((acc, name, index) => {
      acc[name] = {
        label: String(name),
        color: colorPalette[index % colorPalette.length],
      }
      return acc
    }, {} as ChartConfig)
  } else {
    finalConfig = config
  }

  return (
    <Box className="flex flex-col">
      <CardContent className="flex-1 pb-0">
        <ChartContainer
          config={finalConfig}
          className="aspect-square max-h-[250px]"
        >
          <PieChart>
            <ChartTooltip
              cursor={false}
              content={<ChartTooltipContent hideLabel />}
            />
            <Pie
              data={data}
              dataKey={dataKey}
              nameKey={nameKey}
              innerRadius={innerRadius}
              strokeWidth={5}
              activeIndex={activeIndex}
              activeShape={activeShape}
            >
              {data.map((entry, index) => (
                <Cell
                  key={`cell-${index.toString()}`}
                  fill={
                    finalConfig[entry[nameKey]]?.color ||
                    colorPalette[index % colorPalette.length]
                  }
                />
              ))}
            </Pie>
          </PieChart>
        </ChartContainer>
      </CardContent>
      <CardFooter className="flex-col gap-2 text-sm" />
    </Box>
  )
}
