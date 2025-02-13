import {
  BarChart,
  Bar,
  CartesianGrid,
  XAxis,
  Tooltip,
  Legend,
} from "recharts"
import {
  Card,
  CardContent,
  CardFooter,
} from "@/components/ui/card"
import {
  ChartContainer,
  ChartLegendContent,
  ChartTooltipContent,
} from "@/components/ui/chart"

export interface Matrix {
  columnHeaders: string[]
  rowHeaders: string[]
  dataRows: number[][]
}

export interface BarChartMatrixProps {
  matrix: Matrix
}

export function BarChartMatrix({ matrix }: BarChartMatrixProps) {
  const flattenedData = matrix.rowHeaders.map((rowHeader, rowIndex) => {
    const rowData: Record<string, number | string> = { row: rowHeader }
    matrix.columnHeaders.forEach((colHeader, colIndex) => {
      rowData[colHeader] = matrix.dataRows[rowIndex][colIndex]
    })
    return rowData
  })

  const defaultColors = ["var(--color-desktop)", "var(--color-mobile)", "var(--color-tertiary)"]

  return (
    <Card>
      <CardContent>
        <ChartContainer config={{ /* optional config */ }}>
          <BarChart width={600} height={300} data={flattenedData}>
            <CartesianGrid strokeDasharray="3 3" vertical={false} />
            <XAxis dataKey="row" tickLine={false} tickMargin={10} axisLine={false} />
            <Tooltip content={<ChartTooltipContent />} />
            <Legend content={<ChartLegendContent />} />
            {matrix.columnHeaders.map((colHeader, idx) => (
              <Bar
                key={colHeader}
                dataKey={colHeader}
                fill={defaultColors[idx % defaultColors.length]}
              />
            ))}
          </BarChart>
        </ChartContainer>
      </CardContent>
      <CardFooter className="flex-col items-start gap-2 text-sm">
      </CardFooter>
    </Card>
  )
}
