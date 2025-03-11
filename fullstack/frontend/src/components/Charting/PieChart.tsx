import { Cell, Pie, PieChart, Sector } from "recharts"
import type { PieSectorDataItem } from "recharts/types/polar/Pie"

import { CardContent, CardFooter } from "@/components/ui/card"
import { TooltipProps } from "recharts"
import {
  type ChartConfig,
  ChartContainer,
  ChartTooltip,
} from "@/components/ui/chart"
import { useColorPalette } from "@/hooks/useColor"
import { Box } from "@chakra-ui/react"
import { Desc } from "./SankeyChart"
import { useState } from "react"

export interface GenericChartDataItem {
  [key: string]: string | number
}

function formatCurrency(value: number) {
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(value)
}

function SingleSliceTooltip({
  active,
  payload,
  hoveredKey,
}: TooltipProps<number, string> & {
  hoveredKey: string | null
}) {
  // If there's no active hover, or no data, or no hovered key, don't show anything
  if (!active || !payload || payload.length === 0 || !hoveredKey) {
    return null
  }

  const hoveredItem = payload[0]

  console.log(hoveredItem)


  return (
    <Box p={2} className="rounded-md bg-black shadow-md ring-1 ring-black/5">
      <p className="mb-2 font-semibold">{hoveredItem.name}</p>

      <div className="flex items-center gap-2 text-sm">
        <span
          className="inline-block h-2 w-2 rounded-full"
          style={{ backgroundColor: hoveredItem.payload.fill }}
        />
        <span className="font-medium">{formatCurrency(hoveredItem.value ?? 0)}</span>
      </div>
    </Box>
  )
}



export interface GenericPieChartProps {
  data: GenericChartDataItem[]
  dataKey: keyof GenericChartDataItem
  nameKey: keyof GenericChartDataItem
  config?: ChartConfig | null
  innerRadius?: number
  activeIndex?: number
  activeShape?: (props: PieSectorDataItem) => JSX.Element
  description?: string
}

export function GenericPieChart({
  description,
  data,
  dataKey,
  nameKey,
  config,
  innerRadius = 60,
  activeIndex = undefined,
  activeShape = ({ outerRadius = 0, ...props }: PieSectorDataItem) => (
    <Sector {...props} outerRadius={outerRadius + 10} />
  ),
}: GenericPieChartProps) {
  const { getColorForName } = useColorPalette()

  let finalConfig: ChartConfig
  if (!config) {
    const uniqueNames = Array.from(new Set(data.map((item) => item[nameKey])))
    finalConfig = uniqueNames.reduce((acc, name) => {
      acc[String(name)] = {
        label: String(name),
        color: getColorForName(String(name)),
      }
      return acc
    }, {} as ChartConfig)
  } else {
    finalConfig = config
  }

  const [hoveredKey, setHoveredKey] = useState<string | null>(null)


  return (
    <Box className="flex flex-col">
      <CardContent className="flex-1 pb-0 align-center">
        <ChartContainer
          config={finalConfig}
          className="aspect-square max-h-[250px] min-w-[250px]"
        >
          <PieChart>
            <ChartTooltip  content={<SingleSliceTooltip hoveredKey={hoveredKey} />} />
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
                  fill={getColorForName(String(entry[nameKey]))}
                  onMouseEnter={() => setHoveredKey(String(entry[nameKey]))}
                  onMouseLeave={() => setHoveredKey(null)}
                />
              ))}
            </Pie>
          </PieChart>
        </ChartContainer>
        <Desc description={description} />
      </CardContent>
      <CardFooter className="flex-col gap-2 text-sm" />
    </Box>
  )
}
