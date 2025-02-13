import { TrendingUp } from "lucide-react"
import { 
  BarChart, 
  Bar, 
  CartesianGrid, 
  XAxis, 
  Tooltip, 
  Legend 
} from "recharts"
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import {
  ChartContainer,
  ChartLegendContent,
  ChartTooltipContent,
} from "@/components/ui/chart"

import type { AggregatedTransactions } from "../../client"

export function BarChartLocal({
  aggregatedData,
}: {
  aggregatedData: AggregatedTransactions
}) {
  const flattenedData = aggregatedData.groups.flatMap((sourceGroup) =>
    sourceGroup.groups.map((group) => ({
      group: group.group_name,
      withdrawals: group.total_withdrawals,
      deposits: group.total_deposits,
    }))
  )

  return (
    <Card>
      <CardHeader>
        <CardTitle>Aggregated Bar Chart</CardTitle>
        <CardDescription>
          Grouped data: Withdrawals & Deposits by group
        </CardDescription>
      </CardHeader>
      <CardContent>
        <ChartContainer config={{ /* if you have a config object */ }}>
          <BarChart width={600} height={300} data={flattenedData}>
            <CartesianGrid strokeDasharray="3 3" vertical={false} />
            <XAxis dataKey="group" tickLine={false} tickMargin={10} axisLine={false} />
            <Tooltip content={<ChartTooltipContent />} />
            <Legend content={<ChartLegendContent />} />
            <Bar
              dataKey="withdrawals"
              stackId="a"
              fill="var(--color-desktop)"
              radius={[0, 0, 4, 4]}
            />
            <Bar
              dataKey="deposits"
              stackId="a"
              fill="var(--color-mobile)"
              radius={[4, 4, 0, 0]}
            />
          </BarChart>
        </ChartContainer>
      </CardContent>
      <CardFooter className="flex-col items-start gap-2 text-sm">
        <div className="flex gap-2 font-medium leading-none">
          Trending up by 5.2% this month <TrendingUp className="h-4 w-4" />
        </div>
        <div className="leading-none text-muted-foreground">
          Aggregated data from transaction groups
        </div>
      </CardFooter>
    </Card>
  )
}
