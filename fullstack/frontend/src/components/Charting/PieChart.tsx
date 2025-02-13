"use client"

import { Pie, PieChart, Sector, Cell } from "recharts"
import { PieSectorDataItem } from "recharts/types/polar/Pie"

import {
  Card,
  CardHeader,
  CardContent,
  CardFooter,
  CardTitle,
} from "@/components/ui/card"
import {
  ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart"
import { Box } from "@chakra-ui/react"

export interface GenericPieChartProps {
  data: Array<Record<string, any>>
  dataKey: string
  nameKey: string
  title: string
  config?: ChartConfig | null
  innerRadius?: number
  activeIndex?: number
  activeShape?: (props: PieSectorDataItem) => JSX.Element
}

export function GenericPieChart({
  data,
  dataKey,
  nameKey,
  title, 
  config,
  innerRadius = 60,
  activeIndex = 0,
  activeShape = ({ outerRadius = 0, ...props }: PieSectorDataItem) => (
    <Sector {...props} outerRadius={outerRadius + 10} />
  )
}: GenericPieChartProps) {
  const defaultPalette = [
    "hsl(var(--chart-1))",
    "hsl(var(--chart-2))",
    "hsl(var(--chart-3))",
    "hsl(var(--chart-4))",
    "hsl(var(--chart-5))",
  ]

  let finalConfig: ChartConfig
  if (!config) {
    const uniqueNames = Array.from(new Set(data.map((item) => item[nameKey])))
    finalConfig = uniqueNames.reduce((acc, name, index) => {
      acc[name] = {
        label: String(name),
        color: defaultPalette[index % defaultPalette.length],
      }
      return acc
    }, {} as ChartConfig)
  } else {
    finalConfig = config
  }


  return (
    <Box className="flex flex-col">
        <CardHeader>
            <CardTitle>{title}</CardTitle>
        </CardHeader>
      <CardContent className="flex-1 pb-0">
        <ChartContainer
          config={finalConfig}
          className=" aspect-square max-h-[250px]"
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
                  key={`cell-${index}`}
                  fill={
                    finalConfig[entry[nameKey]]?.color ||
                    defaultPalette[index % defaultPalette.length]
                  }
                />
              ))}
            </Pie>
          </PieChart>
        </ChartContainer>
      </CardContent>
      <CardFooter className="flex-col gap-2 text-sm">
      </CardFooter>
    </Box>
  )
}
