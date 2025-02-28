import BoxWithText from "@/components/Common/BoxWithText"
import { isLoggedIn } from "@/hooks/useAuth"
import {
  Box,
  Button,
  Center,
  Flex,
  Grid,
  HStack,
  Spinner,
  Text,
} from "@chakra-ui/react"
import { useQuery } from "@tanstack/react-query"
import { Link } from "@tanstack/react-router"
import { useEffect, useRef, useState } from "react"
import { FiSettings } from "react-icons/fi"
import {
  type AggregatedGroup,
  type GroupByOption,
  type SankeyGetSankeyDataResponse,
  SankeyService,
  type TransactionSourceGroup,
} from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericPieChart } from "../Charting/PieChart"
import { GenericSankeyChart } from "../Charting/SankeyChart"

interface VisualizationProps {
  sourceGroup: TransactionSourceGroup | undefined
  isLoading: boolean
  showDeposits: boolean
}

interface ValidatedVisualizationProps {
  sourceGroup: TransactionSourceGroup
  showDeposits: boolean
}

export function VisualizationPanel({
  showDeposits,
  sourceGroup,
  isLoading,
}: VisualizationProps) {
  return (
    <Flex
      direction="column"
      gap={4}
      minH="300px"
      mb={4}
      align="center"
      justify="center"
    >
      {isLoading || !sourceGroup ? (
        <Spinner size="lg" />
      ) : (
        <Grid
          templateAreas={`"sankey sankey sankey sankey"
                  "pie bar bar bar"`}
          templateColumns="1fr 1fr 1fr 1fr"
          templateRows="auto auto"
          gap={4}
          w="100%"
        >
          <Box gridArea="sankey" width="100%" position="relative">
            <SankeyBox />
          </Box>
          <Box gridArea="pie">
            <PieBox sourceGroup={sourceGroup} showDeposits={showDeposits} />
          </Box>
          <Box gridArea="bar">
            <BarChart sourceGroup={sourceGroup} showDeposits={showDeposits} />
          </Box>
        </Grid>
      )}
    </Flex>
  )
}

function BarChart({ sourceGroup, showDeposits }: ValidatedVisualizationProps) {
  const [isExpanded, setIsExpanded] = useState(true)
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

  const description = `${sourceGroup.transaction_source_name} ${
    showDeposits ? "deposits" : "withdrawals"
  }, by ${sourceGroup.groups[0].groupby_kind} ${
    sourceGroup.groups[0].subgroups?.length
      ? `then ${sourceGroup.groups[0].subgroups[0].groupby_kind}`
      : ""
  }`

  return (
    <BoxWithText
      text={description}
      isExpanded={isExpanded}
      setIsExpanded={setIsExpanded}
    >
      {hasValidTimeGrouping ? (
        <GenericBarChart
          data={chartData}
          dataKey="date"
          nameKey="date"
        />
      ) : (
        <Box textAlign="center" p={4}>
          <Text fontSize="lg" color="gray.500">
            This grouping configuration does not support a bar chart. Please
            include a time-based grouping (e.g., month or year).
          </Text>
        </Box>
      )}
    </BoxWithText>
  )
}

function PieBox({ sourceGroup, showDeposits }: ValidatedVisualizationProps) {
  const [isExpanded, setIsExpanded] = useState(true)
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

  let description: string
  if (
    !sourceGroup.groups[0].subgroups ||
    sourceGroup.groups[0].subgroups.length === 0
  ) {
    description = `${sourceGroup.transaction_source_name}`
  } else {
    description = `${sourceGroup.transaction_source_name} by ${sourceGroup.groups[0].subgroups[0].groupby_kind}`
  }
  return (
    <BoxWithText
      text={description}
      isExpanded={isExpanded}
      setIsExpanded={setIsExpanded}
    >
      <GenericPieChart
        data={chartData}
        dataKey="amount"
        nameKey="group"
        config={null}
      />
    </BoxWithText>
  )
}

export function SankeyBox() {
  const { data, isLoading, error } = useQuery<
    SankeyGetSankeyDataResponse,
    Error
  >({
    queryKey: ["sankeyData"],
    queryFn: () => SankeyService.getSankeyData(),
    enabled: isLoggedIn(),
  })

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

  if (error) {
    setIsExpanded(false)
  }

  const description = "All time transactions grouped by category"

  return (
    <BoxWithText
      text=""
      containerRef={containerRef}
      isExpanded={isExpanded}
      setIsExpanded={setIsExpanded}
      minH={isExpanded ? 300 : undefined}
    >
      <HStack gap={0}>
        <Link to="/sankey-config" href="/sankey-config/">
          <Button variant="outline" alignSelf="start">
            <FiSettings />
          </Button>
        </Link>
      </HStack>

      {isLoading ? (
        <Center flex="1">
          <Spinner size="lg" />
        </Center>
      ) : error ? (
        <Center flex="1">
          <Text color="red.500">
            Failed to load Sankey data. Have you created a config?
          </Text>
        </Center>
      ) : isExpanded && data ? (
        <GenericSankeyChart
          data={data}
          width={chartWidth}
          description={description}
        />
      ) : null}
    </BoxWithText>
  )
}
