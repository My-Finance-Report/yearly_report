import { Card, CardContent, CardFooter } from "@/components/ui/card";
import {
  type ChartConfig,
  ChartContainer,
  ChartTooltip,
} from "@/components/ui/chart";
import { useColorPalette } from "@/hooks/useColor";
import { Box } from "@chakra-ui/react";
import * as React from "react";
import { Bar, BarChart, CartesianGrid, XAxis, YAxis } from "recharts";
import type { TooltipProps } from "recharts";
import type { GenericChartDataItem } from "./PieChart";
import { Desc } from "./SankeyChart";

export interface GenericBarChartProps {
  data: GenericChartDataItem[];
  nameKey: keyof GenericChartDataItem;
  config?: ChartConfig | null;
  description?: string;
}

function formatCurrency(value: number) {
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(value);
}

function formatNumber(value: number) {
  if (value === 0) return "0";

  const absValue = Math.abs(value);
  if (absValue >= 1e9) {
    return (value / 1e9).toFixed(1) + "B";
  }
  if (absValue >= 1e6) {
    return (value / 1e6).toFixed(1) + "M";
  }
  if (absValue >= 1e3) {
    return (value / 1e3).toFixed(1) + "k";
  }
  return value.toString();
}

function SingleSliceTooltip({
  active,
  label,
  payload,
  hoveredKey,
}: TooltipProps<number, string> & {
  hoveredKey: string | null;
}) {
  if (!active || !payload || !payload.length || !hoveredKey) {
    return null;
  }

  const hoveredItem = payload.find((item) => item.dataKey === hoveredKey);
  if (!hoveredItem) {
    return null;
  }

  return (
    <Box
      p={2}
      className="rounded-md bg-black p-3 shadow-md ring-1 ring-black/5"
    >
      <p className="mb-2 font-semibold" style={{ color: "white" }}>
        {label}
      </p>

      <div className="flex items-center gap-2 text-sm">
        <span
          className="inline-block h-2 w-2 rounded-full"
          style={{ backgroundColor: hoveredItem.color }}
        />
        <span style={{ color: "white" }}>{hoveredItem.name}:</span>
        <span className="font-medium" style={{ color: "white" }}>
          {formatCurrency(hoveredItem.value || 0)}
        </span>
      </div>
    </Box>
  );
}

export function GenericBarChart({
  data,
  nameKey,
  config,
  description,
}: GenericBarChartProps) {
  const { getColorForName } = useColorPalette();

  const uniqueKeys = Array.from(
    new Set(data.flatMap((obj) => Object.keys(obj))),
  ).filter((key) => key !== nameKey && key !== "date");

  const [hoveredKey, setHoveredKey] = React.useState<string | null>(null);

  const computedConfig: ChartConfig = React.useMemo(() => {
    if (config) return config;

    return uniqueKeys.reduce((acc, key) => {
      acc[key] = {
        label: key.charAt(0).toUpperCase() + key.slice(1),
        color: getColorForName(key),
      };
      return acc;
    }, {} as ChartConfig);
  }, [config, getColorForName, uniqueKeys]);

  return (
    <>
      <Card>
        <CardContent
          className="px-2 sm:p-6"
          style={{ backgroundColor: "background", padding: 10 }}
        >
          <ChartContainer
            config={computedConfig}
            className="aspect-auto h-[250px] w-full"
          >
            <BarChart
              data={data}
              margin={{ top: 10, right: 0, bottom: 50, left: 0 }}
            >
              <CartesianGrid vertical={false} />
              <XAxis
                dataKey={nameKey}
                tickLine={false}
                axisLine={false}
                interval={0}
                //@ts-expect-error tick is not defined in the type but exists
                tick={{ angle: -90, textAnchor: "end", textSize: 12 }}
                tickFormatter={(value: string) => {
                  const maxLength = 20;
                  if (value.length > maxLength) {
                    return `${value.slice(0, maxLength)}...`;
                  }
                  return value;
                }}
              />
              <YAxis
                tickLine={false}
                axisLine={false}
                tickMargin={8}
                tickCount={3}
                tickFormatter={(value) => formatNumber(value)}
              />
              <ChartTooltip
                content={<SingleSliceTooltip hoveredKey={hoveredKey} />}
              />
              {uniqueKeys.map((key, index) => (
                <Bar
                  key={key}
                  dataKey={key}
                  stackId="a"
                  fill={getColorForName(key) || "gray"}
                  onMouseEnter={() => setHoveredKey(key)}
                  onMouseLeave={() => setHoveredKey(null)}
                  radius={
                    index === uniqueKeys.length - 1
                      ? [4, 4, 0, 0]
                      : [0, 0, 0, 0]
                  }
                />
              ))}
            </BarChart>
          </ChartContainer>
          <Desc description={description} />
        </CardContent>
        <CardFooter className="flex-col gap-2 text-sm" />
      </Card>
    </>
  );
}
