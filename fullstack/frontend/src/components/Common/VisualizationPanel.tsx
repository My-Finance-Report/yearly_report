import BoxWithText, {
  type CollapsibleName,
} from "@/components/Common/BoxWithText"
import { useIsDesktop, useIsMobile } from "@/hooks/useIsMobile"
import { Box, Flex, Spinner, Text } from "@chakra-ui/react"
import type { AggregatedGroup } from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import {
  type GenericChartDataItem,
  GenericPieChart,
} from "../Charting/PieChart"

interface VisualizationProps {
  sourceGroups: AggregatedGroup[] | null
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

  const isDesktop = useIsDesktop()

  if (!sourceGroups) {
    return null
  }

  console.log(isDesktop)

  return (
    <Flex
      direction="column"
      gap={4}
      minWidth="100%"
      align="center"
      justify="center"
    >
      {isLoading ? (
        <Spinner size="lg" />
      ) : (
        <Flex
          direction={isDesktop ? "row" : "column"}
          wrap="wrap"
          gap={3}
          width="100%"
          justify="center"
        >
          {isDesktop && !collapsedItems.includes("Pie Chart") && (
            <Box maxWidth="280px" flex="1">
              <PieBox
                sourceGroups={sourceGroups}
                showDeposits={showDeposits}
                collapsedItems={collapsedItems}
                setCollapsedItems={setCollapsedItems}
              />
            </Box>
          )}

          <Box minW="300px" flex="1">
            <BarChart
              sourceGroups={sourceGroups}
              showDeposits={showDeposits}
              collapsedItems={collapsedItems}
              setCollapsedItems={setCollapsedItems}
            />
          </Box>
        </Flex>
      )}
    </Flex>
  )
}

function squareOffData(data: GenericChartDataItem[]) {
  const allKeys: Set<string> = new Set()
  data.forEach((row: GenericChartDataItem) => {
    Object.keys(row).forEach((key) => {
      if (key !== "date") {
        allKeys.add(key)
      }
    })
  })

  const allKeysArray: string[] = Array.from(allKeys)
  return data.map((row) => {
    const newRow: GenericChartDataItem = { ...row }
    allKeysArray.forEach((key) => {
      if (!(key in newRow)) {
        newRow[key] = 0
      }
    })
    return newRow
  })
}

function BarChart({
  sourceGroups,
  showDeposits,
  collapsedItems,
  setCollapsedItems,
}: ValidatedVisualizationProps) {
  const categoryKeys = new Set<string>()
  for (const group of sourceGroups) {
    if (group.subgroups && group.subgroups.length > 0) {
      for (const subgroup of group.subgroups) {
        categoryKeys.add(subgroup.group_name)
      }
    } else {
      categoryKeys.add(group.group_name)
    }
  }

  const chartData: GenericChartDataItem[] = sourceGroups.map((group) => {
    const base: Record<string, number | string> = {
      date: group.group_id.toString(),
    }

    if (group.subgroups && group.subgroups.length > 0) {
      for (const subgroup of group.subgroups) {
        base[subgroup.group_name] = showDeposits
          ? subgroup.total_deposits
          : subgroup.total_withdrawals
      }
    } else {
      base[group.group_name] = showDeposits
        ? group.total_deposits
        : group.total_withdrawals
    }

    return base
  })

  const description = `${showDeposits ? "Deposits" : "Expenses"} by ${
    sourceGroups[0].groupby_kind
  } ${
    sourceGroups[0].subgroups?.length
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
  const chartData: Record<string, number> = {}

  sourceGroups.forEach((group) => {
    if (group.subgroups && group.subgroups.length > 0) {
      group.subgroups.forEach((subgroup) => {
        const amount = showDeposits
          ? subgroup.total_deposits
          : subgroup.total_withdrawals
        chartData[subgroup.group_name] =
          (chartData[subgroup.group_name] || 0) + amount
      })
    } else {
      const amount = showDeposits
        ? group.total_deposits
        : group.total_withdrawals
      chartData[group.group_name] = (chartData[group.group_name] || 0) + amount
    }
  })

  const description = `${showDeposits ? "Deposits" : "Expenses"} by ${
    sourceGroups[0]?.groupby_kind || "Group"
  }`

  if (Object.keys(chartData).length === 0) return null

  return (
    <BoxWithText
      text={""}
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
      COMPONENT_NAME="Pie Chart"
    >
      <GenericPieChart
        data={Object.entries(chartData).map(([key, value]) => ({
          name: key,
          value,
        }))}
        dataKey="value"
        description={description}
        nameKey="name"
        config={null}
      />
    </BoxWithText>
  )
}
