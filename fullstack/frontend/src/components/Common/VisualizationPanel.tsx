import { SegmentedControl } from "@/components/ui/segmented-control"
import { Box, Flex, Grid, HStack, Spinner, Text } from "@chakra-ui/react"
import { useEffect, useRef, useState } from "react"
import {
  FiArrowDown,
  FiArrowUp,
  FiDownload,
  FiInbox,
  FiLogIn,
  FiLogOut,
  FiUpload,
} from "react-icons/fi"
import type {
  AggregatedGroup,
  GroupByOption,
  TransactionSourceGroup,
} from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericPieChart } from "../Charting/PieChart"
import { GenericSankeyChart } from "../Charting/SankeyChart"

interface VisualizationProps {
  activeSlice: { [sourceId: number]: number }
  sourceGroup: TransactionSourceGroup | undefined
  isLoading: boolean
}

interface ValidatedVisualizationProps {
  activeSlice: { [sourceId: number]: number }
  sourceGroup: TransactionSourceGroup
  showDeposits: boolean
}

export function VisualizationPanel({
  activeSlice,
  sourceGroup,
  isLoading,
}: VisualizationProps) {
  const [showDeposits, setShowDeposits] = useState(false)

  const items = [
    {
      value: "deposits",
      label: (
        <HStack>
          <Box as={FiLogIn} />
          Deposits
        </HStack>
      ),
    },
    {
      value: "withdrawals",
      label: (
        <HStack>
          <Box as={FiLogOut} />
          Withdrawals
        </HStack>
      ),
    },
  ]

  return (
    <Flex
      direction="column"
      gap={4}
      minH="300px"
      align="center"
      justify="center"
    >
      <SegmentedControl
        defaultValue={"withdrawals"}
        value={showDeposits ? "deposits" : "withdrawals"}
        items={items}
        onValueChange={(value) => setShowDeposits(value.value === "deposits")}
      />

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
            <PieBox sourceGroup={sourceGroup} activeSlice={activeSlice} showDeposits={showDeposits} />
          </Box>

          <Box gridArea="bar">
            <BarChart sourceGroup={sourceGroup} activeSlice={activeSlice} showDeposits={showDeposits} />
          </Box>

          <Box gridArea="sankey" width="100%">
            <SankeyBox sourceGroup={sourceGroup} showDeposits={showDeposits} />
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

function PieBox({
  sourceGroup,
  activeSlice,
  showDeposits,
}: ValidatedVisualizationProps) {
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
        acc[group] = (acc[group] || 0) + amount // Sum amounts for duplicate keys
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
        activeIndex={activeSlice[sourceGroup.transaction_source_id] ?? 0}
      />
    </Box>
  )
}

function SankeyBox({
  sourceGroup,
  showDeposits,
}: { sourceGroup: TransactionSourceGroup; showDeposits: boolean }) {
  const nodes: { name: string; id: number }[] = []
  const links: { source: number; target: number; value: number }[] = []
  const nodeIndexMap = new Map<string, number>()

  for (const [idx, name] of ["deposits", "withdrawals"].entries()) {
    nodes.push({ name, id: idx })
    nodeIndexMap.set(name, idx)
  }

  let nextNodeId = nodes.length

  for (const group of sourceGroup.groups) {
    let parentId = nodeIndexMap.get(group.group_name)

    if (parentId === undefined) {
      nodes.push({ name: group.group_name, id: nextNodeId })
      nodeIndexMap.set(group.group_name, nextNodeId)
      parentId = nextNodeId
      nextNodeId++
    }

    if (group.subgroups) {
      for (const subgroup of group.subgroups) {
        let subgroupId = nodeIndexMap.get(subgroup.group_name)

        if (subgroupId === undefined) {
          nodes.push({ name: subgroup.group_name, id: nextNodeId })
          nodeIndexMap.set(subgroup.group_name, nextNodeId)
          subgroupId = nextNodeId
          nextNodeId++
        }

        links.push({
          source: parentId,
          target: subgroupId,
          value: showDeposits
            ? subgroup.total_deposits
            : subgroup.total_withdrawals,
        })
      }
    }
  }

  const containerRef = useRef<HTMLDivElement | null>(null)
  const [chartWidth, setChartWidth] = useState<number>(0)

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


  console.log(chartWidth)

  return (
    <Box flex="1" minW="50%" borderWidth={1} borderRadius="md" ref={containerRef}>
      <GenericSankeyChart data={{ nodes, links }} width={chartWidth} />
    </Box>
  )
}
