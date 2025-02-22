import { Box, Button, Flex, Grid, Spinner, Text } from "@chakra-ui/react"
import { useEffect, useRef, useState } from "react"
import type {
  AggregatedGroup,
  AggregatedTransactions,
  GroupByOption,
  TransactionSourceGroup,
} from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericPieChart } from "../Charting/PieChart"
import { GenericSankeyChart } from "../Charting/SankeyChart"

interface VisualizationProps {
  sourceGroup: TransactionSourceGroup | undefined
  isLoading: boolean
  showDeposits: boolean
  data: AggregatedTransactions
}

interface ValidatedVisualizationProps {
  sourceGroup: TransactionSourceGroup
  showDeposits: boolean
}

export function VisualizationPanel({
  showDeposits,
  sourceGroup,
  isLoading,
  data,
}: VisualizationProps) {
  return (
    <Flex
      direction="column"
      gap={4}
      minH="300px"
      align="center"
      justify="center"
    >
      {isLoading || !sourceGroup ? (
        <Spinner size="lg" />
      ) : (
        <Grid
          templateAreas={`"pie bar bar bar"
                  "sankey sankey sankey sankey"`}
          templateColumns="1fr 1fr 1fr 1fr"
          templateRows="auto auto"
          gap={4}
          w="100%"
        >
          <Box gridArea="pie">
            <PieBox sourceGroup={sourceGroup} showDeposits={showDeposits} />
          </Box>
          <Box gridArea="bar">
            <BarChart sourceGroup={sourceGroup} showDeposits={showDeposits} />
          </Box>
          <Box gridArea="sankey" width="100%" position="relative">
            <SankeyBox data={data} />
          </Box>
        </Grid>
      )}
    </Flex>
  )
}

function BarChart({ sourceGroup, showDeposits }: ValidatedVisualizationProps) {
  const TIME_OPTIONS: GroupByOption[] = ["month", "year"]

  const hasTimeGrouping = (group: AggregatedGroup): boolean => {
    if (group.groupby_kind && TIME_OPTIONS.includes(group.groupby_kind)) {
      return true
    }
    return group.subgroups?.some(hasTimeGrouping) || false
  }

  const hasValidTimeGrouping = sourceGroup.groups.some(hasTimeGrouping)

  const categoryKeys = new Set<string>()
  for (const group of sourceGroup.groups) {
    if (group.subgroups) {
      for (const subgroup of group.subgroups) {
        categoryKeys.add(subgroup.group_name)
      }
    }
  }

  const chartData = hasValidTimeGrouping
    ? sourceGroup.groups.map((group) => {
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
    : []

  return (
    <Box flex="1" minW="50%" borderWidth={1} borderRadius="md">
      {hasValidTimeGrouping ? (
        <GenericBarChart data={chartData} dataKey="date" nameKey="date" />
      ) : (
        <Box textAlign="center" p={4}>
          <Text fontSize="lg" color="gray.500">
            This grouping configuration does not support a bar chart. Please
            include a time-based grouping (e.g., month or year).
          </Text>
        </Box>
      )}
    </Box>
  )
}

function PieBox({ sourceGroup, showDeposits }: ValidatedVisualizationProps) {
  const chartDataMap = sourceGroup.groups
    .flatMap(
      (group) =>
        group.subgroups?.map((subgroup) => ({
          group: subgroup.group_name,
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
        ],
    )
    .reduce(
      (acc, { group, amount }) => {
        acc[group] = (acc[group] || 0) + amount
        return acc
      },
      {} as Record<string, number>,
    )

  const chartData = Object.entries(chartDataMap).map(([group, amount]) => ({
    group,
    amount,
  }))

  return (
    <Box flex="1" minW="50%" borderWidth={1} borderRadius="md">
      <GenericPieChart
        data={chartData}
        dataKey="amount"
        nameKey="group"
        config={null}
      />
    </Box>
  )
}

function generateSankeyData(data: AggregatedTransactions, config: any) {
  const nodes: { name: string; id: number }[] = []
  const links: { source: number; target: number; value: number }[] = []
  const nodeIndexMap = new Map<string, number>()

  let nextNodeId = 0

  // Add sources as nodes
  data.groups.forEach((sourceGroup) => {
    if (!nodeIndexMap.has(sourceGroup.transaction_source_name)) {
      nodeIndexMap.set(sourceGroup.transaction_source_name, nextNodeId)
      nodes.push({ name: sourceGroup.transaction_source_name, id: nextNodeId })
      nextNodeId++
    }

    // Add direct inputs from config
    config.inputs.forEach(({ source, target }) => {
      if (sourceGroup.transaction_source_name === source) {
        if (!nodeIndexMap.has(target)) {
          nodeIndexMap.set(target, nextNodeId)
          nodes.push({ name: target, id: nextNodeId })
          nextNodeId++
        }

        links.push({
          source: nodeIndexMap.get(source)!,
          target: nodeIndexMap.get(target)!,
          value: sourceGroup.total_deposits,
        })
      }
    })
  })

  config.linkages.forEach(({ source, intermediary, target }) => {
    if (!nodeIndexMap.has(source)) return
    if (!nodeIndexMap.has(intermediary)) {
      nodeIndexMap.set(intermediary, nextNodeId)
      nodes.push({ name: intermediary, id: nextNodeId })
      nextNodeId++
    }
    if (!nodeIndexMap.has(target)) {
      nodeIndexMap.set(target, nextNodeId)
      nodes.push({ name: target, id: nextNodeId })
      nextNodeId++
    }

    links.push({
      source: nodeIndexMap.get(source)!,
      target: nodeIndexMap.get(intermediary)!,
      value: data.overall_deposits / 2,
    })

    links.push({
      source: nodeIndexMap.get(intermediary)!,
      target: nodeIndexMap.get(target)!,
      value: data.overall_deposits / 2,
    })
  })

  return { nodes, links }
}

function SankeyBox({
  data,
}: { data: AggregatedTransactions }) {
  const { nodes, links } = generateSankeyData(data)


  const containerRef = useRef<HTMLDivElement | null>(null)
  const [chartWidth, setChartWidth] = useState<number>(0)
  const [isExpanded, setIsExpanded] = useState(true)

  useEffect(() => {
    const updateWidth = () => {
      if (containerRef.current) {
        setChartWidth(containerRef.current.offsetWidth - 30)
      }
    }

    updateWidth()
    window.addEventListener("resize", updateWidth)

    return () => {
      window.removeEventListener("resize", updateWidth)
    }
  }, [])

  return (
    <Box
      flex="1"
      minW="50%"
      borderWidth={1}
      borderRadius="md"
      ref={containerRef}
    >
      <Button
        variant={"outline"}
        onClick={() => setIsExpanded((prev) => !prev)}
      >
        {isExpanded ? "Collapse" : "Expand"} Flowchart
      </Button>
      {isExpanded && (
        <GenericSankeyChart data={{ nodes, links }} width={chartWidth} />
      )}
    </Box>
  )
}
