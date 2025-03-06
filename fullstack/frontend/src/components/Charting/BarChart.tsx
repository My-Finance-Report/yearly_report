import { Card, CardContent, CardFooter } from "@/components/ui/card"
import {
  type ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart"
import { useColorPalette } from "@/hooks/useColor"
import * as React from "react"
import { Bar, BarChart, CartesianGrid, XAxis, YAxis } from "recharts"
import type { GenericChartDataItem } from "./PieChart"
import { Desc } from "./SankeyChart"

export interface GenericBarChartProps {
  data: GenericChartDataItem[]
  dataKey: keyof GenericChartDataItem
  nameKey: keyof GenericChartDataItem
  config?: ChartConfig | null
  description?: string
}

export function GenericBarChart({
  data,
  nameKey,
  config,
  description,
}: GenericBarChartProps) {
  const { getColorForName } = useColorPalette()

  const uniqueKeys = Object.keys(data[0] || {}).filter(
    (key) => key !== nameKey && key !== "date",
  )

  const computedConfig: ChartConfig = React.useMemo(() => {
    if (config) return config

    return uniqueKeys.reduce((acc, key, index) => {
      acc[key] = {
        label: key.charAt(0).toUpperCase() + key.slice(1),
        color: getColorForName(key),
      }
      return acc
    }, {} as ChartConfig)
  }, [config, getColorForName, uniqueKeys])

  return (
    <Card>
      <CardContent
        className="px-2 sm:p-6"
        style={{ backgroundColor: "background" }}
      >
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
            <YAxis
              tickLine={false}
              axisLine={false}
              tickMargin={8}
              tickCount={3}
            />
            <ChartTooltip
              content={<ChartTooltipContent hideIndicator={false} />}
            />
            {uniqueKeys.map((key, index) => (
              <Bar
                key={key}
                dataKey={key}
                stackId="a"
                fill={getColorForName(key) || "gray"}
                radius={
                  index === uniqueKeys.length - 1 ? [4, 4, 0, 0] : [0, 0, 0, 0]
                }
              />
            ))}
          </BarChart>
        </ChartContainer>
        <Desc description={description} />
      </CardContent>
      <CardFooter className="flex-col gap-2 text-sm" />
    </Card>
  )
}
