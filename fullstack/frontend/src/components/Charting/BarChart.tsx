import { Card, CardContent } from "@/components/ui/card"
import {
  type ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart"
import { useTheme } from "next-themes"
import * as React from "react"
import { Bar, BarChart, CartesianGrid, XAxis } from "recharts"
import type { GenericChartDataItem } from "./PieChart"
import useColorPalette from "@/hooks/useColorPalette"

export interface GenericBarChartProps {
  data: GenericChartDataItem[]
  dataKey: keyof GenericChartDataItem
  nameKey: keyof GenericChartDataItem
  config?: ChartConfig | null
}

export function GenericBarChart({
  data,
  nameKey,
  config,
}: GenericBarChartProps) {
  const colorPalette = useColorPalette()

  const uniqueKeys = Object.keys(data[0] || {}).filter(
    (key) => key !== nameKey && key !== "date",
  )

  const computedConfig: ChartConfig = React.useMemo(() => {
    if (config) return config

    return uniqueKeys.reduce((acc, key, index) => {
      acc[key] = {
        label: key.charAt(0).toUpperCase() + key.slice(1),
        color: colorPalette[index % colorPalette.length],
      }
      return acc
    }, {} as ChartConfig)
  }, [config, colorPalette, uniqueKeys])

  return (
    <Card>
      <CardContent className="px-2 sm:p-6">
        <ChartContainer
          config={computedConfig}
          className="aspect-auto h-[250px] w-full"
        >
          <BarChart data={data} margin={{ left: 12, right: 12 }}>
            <CartesianGrid vertical={false} />
            <XAxis
              dataKey="date"
              tickLine={false}
              axisLine={false}
              tickMargin={8}
              minTickGap={32}
              tickFormatter={(value) => value}
            />
            <ChartTooltip
              content={
                <ChartTooltipContent
                  className="w-[150px]"
                  nameKey="date"
                  labelFormatter={(value) => value}
                />
              }
            />
            {uniqueKeys.map((key, index) => (
              <Bar
                key={key}
                dataKey={key}
                stackId="a"
                fill={computedConfig[key]?.color || "gray"}
                radius={
                  index === uniqueKeys.length - 1 ? [4, 4, 0, 0] : [0, 0, 0, 0]
                }
              />
            ))}
          </BarChart>
        </ChartContainer>
      </CardContent>
    </Card>
  )
}
