import {
  Box,
  Flex,
  Grid,
  HStack,
  Spinner,
  Text,
} from "@chakra-ui/react"
import { SegmentedControl } from "@/components/ui/segmented-control"
import type {
  AggregatedGroup,
  GroupByOption,
  TransactionSourceGroup,
} from "../../client"
import { FiUpload, FiDownload, FiArrowDown, FiArrowUp, FiInbox, FiLogOut, FiLogIn } from "react-icons/fi"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericPieChart } from "../Charting/PieChart"
import { GenericSankeyChart } from "../Charting/SankeyChart"
import { useState } from "react"

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



  const items =
    [

      {
        value: "deposits", label:
          (<HStack>
            <Box as={FiLogIn} />
            Deposits
          </HStack>
          )
      },
      {
        value: "withdrawals", label:
          (<HStack>
            <Box as={FiLogOut} />
            Withdrawals
          </HStack>
          )
      }
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
        defaultValue={'withdrawals'}
        value={showDeposits ? "deposits" : "withdrawals"}
        items={items}
        onValueChange={(value) => setShowDeposits(value.value === 'deposits')}
      />

      {isLoading || !sourceGroup ? (
        <Spinner size="lg" />
      ) : (
        <Grid templateColumns="repeat(2, 1fr)" gap={4} w="100%">
          <PieBox
            sourceGroup={sourceGroup}
            activeSlice={activeSlice}
            showDeposits={showDeposits}
          />
          <BarChart
            sourceGroup={sourceGroup}
            activeSlice={activeSlice}
            showDeposits={showDeposits}
          />
          <SankeyBox sourceGroup={sourceGroup} showDeposits={showDeposits} />
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
    <Box flex="1" minW="50%">
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
    <Box flex="1" minW="50%">
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

  // Initialize the first nodes
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

  return (
    <Box flex="1" minW="50%">
      <GenericSankeyChart data={{ nodes, links }} />
    </Box>
  )
}
