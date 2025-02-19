import * as React from "react";
import { Bar, BarChart, CartesianGrid, XAxis } from "recharts";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import {
  ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart";
import { useTheme } from "next-themes";

export interface GenericBarChartProps {
  data: Array<Record<string, any>>;
  dataKey: string;
  nameKey: string;
  title: string;
  config?: ChartConfig | null;
  innerRadius?: number;
  activeIndex?: number;
}

export function GenericBarChart({
  data,
  dataKey,
  nameKey,
  title,
  config,
}: GenericBarChartProps) {
  const theme = useTheme();

  const lightModePalette = ["#3182CE", "#38A169", "#E53E3E", "#DD6B20", "#805AD5"];
  const darkModePalette = ["#63B3ED", "#68D391", "#FC8181", "#F6AD55", "#B794F4"];

  const colorPalette = theme.theme === "dark" ? darkModePalette : lightModePalette;



  const [activeChart, setActiveChart] = React.useState<string | null>(null);

  const computedConfig: ChartConfig = React.useMemo(() => {
    if (config) return config;

    // Dynamically generate config if not provided
    const uniqueKeys = Object.keys(data[0] || {}).filter(
      (key) => key !== nameKey
    );

    return uniqueKeys.reduce((acc, key, index) => {
      acc[key] = {
        label: key.charAt(0).toUpperCase() + key.slice(1),
        color: colorPalette[index % colorPalette.length],
      };
      return acc;
    }, {} as ChartConfig);
  }, [data, config, theme, nameKey]);

  // Determine initial active chart key
  React.useEffect(() => {
    if (!activeChart && Object.keys(computedConfig).length > 0) {
      setActiveChart(Object.keys(computedConfig)[0]);
    }
  }, [computedConfig, activeChart]);

  // Compute total values dynamically
  const total = React.useMemo(() => {
    return activeChart
      ? data.reduce((acc, curr) => acc + (curr[activeChart] || 0), 0)
      : 0;
  }, [data, activeChart]);

  return (
    <Card>
      <CardHeader className="flex flex-col items-stretch space-y-0 border-b p-0 sm:flex-row">
        <div className="flex flex-1 flex-col justify-center gap-1 px-6 py-5 sm:py-6">
          <CardTitle>{title}</CardTitle>
          <CardDescription>Showing total for selected metric</CardDescription>
        </div>
        <div className="flex">
          {Object.keys(computedConfig).map((key) => (
            <button
              key={key}
              data-active={activeChart === key}
              className={`relative z-30 flex flex-1 flex-col justify-center gap-1 border-t px-6 py-4 text-left even:border-l ${activeChart === key ? "bg-muted/50" : ""
                } sm:border-l sm:border-t-0 sm:px-8 sm:py-6`}
              onClick={() => setActiveChart(key)}
            >
              <span className="text-xs text-muted-foreground">
                {computedConfig[key].label}
              </span>
              <span className="text-lg font-bold leading-none sm:text-3xl">
                {total.toLocaleString()}
              </span>
            </button>
          ))}
        </div>
      </CardHeader>
      <CardContent className="px-2 sm:p-6">
        <ChartContainer
          config={computedConfig}
          className="aspect-auto h-[250px] w-full"
        >
          <BarChart
            data={data}
            margin={{
              left: 12,
              right: 12,
            }}
          >
            <CartesianGrid vertical={false} />
            <XAxis
              dataKey={nameKey}
              tickLine={false}
              axisLine={false}
              tickMargin={8}
              minTickGap={32}
              tickFormatter={(value) =>
                new Date(value).toLocaleDateString("en-US", {
                  month: "short",
                  day: "numeric",
                })
              }
            />
            <ChartTooltip
              content={
                <ChartTooltipContent
                  className="w-[150px]"
                  nameKey={dataKey}
                  labelFormatter={(value) =>
                    new Date(value).toLocaleDateString("en-US", {
                      month: "short",
                      day: "numeric",
                      year: "numeric",
                    })
                  }
                />
              }
            />
            {activeChart && (
              <Bar
                dataKey={activeChart}
                fill={computedConfig[activeChart]?.color || "gray"}
              />
            )}
          </BarChart>
        </ChartContainer>
      </CardContent>
    </Card>
  );
}
