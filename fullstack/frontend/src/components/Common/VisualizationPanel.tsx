import BoxWithText, {
  type CollapsibleName,
} from "@/components/Common/BoxWithText"
import {
  Box,
  Flex,
  Grid,
  Spinner,
  Text,
} from "@chakra-ui/react"
import {
  type AggregatedGroup,
} from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericChartDataItem, GenericPieChart } from "../Charting/PieChart"
import { useIsMobile } from "@/hooks/useIsMobile"

interface VisualizationProps {
  sourceGroups: AggregatedGroup[] 
  isLoading: boolean
  showDeposits: boolean
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  collapsedItems: CollapsibleName[]
}

interface ValidatedVisualizationProps {
  sourceGroups: AggregatedGroup[]
  showDeposits: boolean
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  collapsedItems: CollapsibleName[]
}

export function VisualizationPanel({
  showDeposits,
  sourceGroups,
  isLoading,
  setCollapsedItems,
  collapsedItems,
}: VisualizationProps) {

  const isMobile = useIsMobile()

  let layout = isMobile ? "bar bar bar bar" : "pie bar bar bar"

  if (collapsedItems.includes("Pie Chart")) {
    layout = "bar bar bar bar"
  }

  return (
    <Flex direction="column" gap={4} mb={4} align="center" justify="center">
      {isLoading || !sourceGroups ? (
        <Spinner size="lg" />
      ) : (
        <Grid
          templateAreas={`"${layout}"`}
          templateColumns="1fr 1fr 1fr 1fr"
          templateRows="auto auto"
          gap={4}
          w="100%"
        >
          {!isMobile && (
              <Box gridArea="pie">
                <PieBox
                  sourceGroups={sourceGroups}
                  showDeposits={showDeposits}
                  collapsedItems={collapsedItems}
                  setCollapsedItems={setCollapsedItems}
                />
              </Box>
          )
          }
          <Box gridArea="bar">
            <BarChart
              sourceGroups={sourceGroups}
              showDeposits={showDeposits}
              collapsedItems={collapsedItems}
              setCollapsedItems={setCollapsedItems}
            />
          </Box>
        </Grid>
      )}
    </Flex>
  )
}

function squareOffData(data: GenericChartDataItem[]) {
  // 1. Collect all unique keys (besides 'date')
  const allKeys: Set<string> = new Set();
  data.forEach((row: GenericChartDataItem) => {
    Object.keys(row).forEach((key) => {
      if (key !== 'date') {
        allKeys.add(key);
      }
    });
  });

  const allKeysArray: string[] = Array.from(allKeys);
  return data.map((row) => {
    const newRow: GenericChartDataItem = { ...row };
    allKeysArray.forEach((key) => {
      if (!(key in newRow)) {
        newRow[key] = 0;
      }
    });
    return newRow;
  });
}

  
function BarChart({
  sourceGroups,
  showDeposits,
  collapsedItems,
  setCollapsedItems,
}: ValidatedVisualizationProps) {

  
  const categoryKeys = new Set<string>()
  for (const group of sourceGroups) {
    if (group.subgroups) {
      for (const subgroup of group.subgroups) {
        categoryKeys.add(subgroup.group_name)
      }
    }
  }

  const chartData: GenericChartDataItem[]  =  sourceGroups.map((group) => {
      const base: Record<string, number | string> = {
        date: group.group_id.toString(),
      }

      if (group.subgroups) {
        for (const subgroup of group.subgroups) {
          base[subgroup.group_name] = showDeposits
            ? subgroup.total_deposits
            : subgroup.total_withdrawals
        }
      }

      return base
    })


  const description = `${showDeposits ? "Deposits" : "Expenses"
    } by ${sourceGroups[0].groupby_kind} ${sourceGroups[0].subgroups?.length
      ? `then ${sourceGroups[0].subgroups[0].groupby_kind}`
      : ""
    }`

  const isMobile = useIsMobile()

  return (
    <BoxWithText
      text={""}
      COMPONENT_NAME="Bar Chart"
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
      isCollapsable={!isMobile}
    >
      {chartData ? (
        <GenericBarChart
          data={squareOffData(chartData)}
          description={description}
          dataKey="date"
          nameKey="date"
        />
      ) : (
        <Box textAlign="center" p={12}>
          <Text fontSize="lg" color="gray.500">
            This grouping configuration does not support a bar chart. Please
            include a time-based grouping (e.g., month or year).
          </Text>
        </Box>
      )}
    </BoxWithText>
  )
}

function PieBox({
  sourceGroups,
  showDeposits,
  setCollapsedItems,
  collapsedItems,
}: ValidatedVisualizationProps) {

const chartDataMap = sourceGroups.flatMap(group =>
  group.subgroups?.map(subgroup => ({
    group: group.group_name, // use the outer group's name
    amount: showDeposits
      ? subgroup.total_deposits
      : subgroup.total_withdrawals,
  })) || [
    {
      group: group.group_name,
      amount: showDeposits
        ? group.total_deposits
        : group.total_withdrawals,
    },
  ]
).reduce(
  (acc, { group, amount }) => {
    acc[group] = (acc[group] || 0) + amount;
    return acc;
  },
  {} as Record<string, number>,
);

  let description: string
  if (
    !sourceGroups[0].subgroups ||
    sourceGroups[0].subgroups.length === 0
  ) {
    description = `${sourceGroups[0].group_name}`
  } else {
    description = `${sourceGroups[0].group_name} by ${sourceGroups[0].subgroups[0].groupby_kind}`
  }


  if (!chartDataMap) return null
  return (
    <BoxWithText
      text={""}
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
      COMPONENT_NAME="Pie Chart"
    >
      <GenericPieChart
        data={Object.entries(chartDataMap).map(([key, value]) => ({ name: key, value }))}
        dataKey="value"
        description={description}
        nameKey="name"
        config={null}
      />
    </BoxWithText>
  )
}

