import BoxWithText, {
  type CollapsibleName,
} from "@/components/Common/BoxWithText"
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
} from "../../client"
import { GenericBarChart } from "../Charting/BarChart"
import { GenericChartDataItem, GenericPieChart } from "../Charting/PieChart"
import { GenericSankeyChart } from "../Charting/SankeyChart"
import { useIsMobile } from "@/hooks/useIsMobile"

interface VisualizationProps {
  sourceGroups: AggregatedGroup[] 
  isLoading: boolean
  showDeposits: boolean
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  includeSankey?: boolean
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
  includeSankey = true,
}: VisualizationProps) {

  const isMobile = useIsMobile()

  const layout = isMobile ? "bar bar bar bar" : "pie bar bar bar"

  return (
    <Flex direction="column" gap={4} mb={4} align="center" justify="center">
      {isLoading || !sourceGroups ? (
        <Spinner size="lg" />
      ) : (
        <Grid
          templateAreas={`"sankey sankey sankey sankey" "${layout}"`}
          templateColumns="1fr 1fr 1fr 1fr"
          templateRows="auto auto"
          gap={4}
          w="100%"
        >
          {!isMobile && (
            <>
            {includeSankey &&
              <Box gridArea="sankey" width="100%" position="relative">
                <SankeyBox
                  sourceGroups={sourceGroups}
                  showDeposits={showDeposits}
                  collapsedItems={collapsedItems}
                  setCollapsedItems={setCollapsedItems}
                />
              </Box>
}
              <Box gridArea="pie">
                <PieBox
                  sourceGroups={sourceGroups}
                  showDeposits={showDeposits}
                  collapsedItems={collapsedItems}
                  setCollapsedItems={setCollapsedItems}
                />
              </Box>
            </>
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

function BarChart({
  sourceGroups,
  showDeposits,
  collapsedItems,
  setCollapsedItems,
}: ValidatedVisualizationProps) {
  const TIME_OPTIONS: GroupByOption[] = ["month", "year"]

  const hasTimeGrouping = (group: AggregatedGroup): boolean => {
    if (group.groupby_kind && TIME_OPTIONS.includes(group.groupby_kind)) {
      return true
    }
    return group.subgroups?.some(hasTimeGrouping) || false
  }

  const hasValidTimeGrouping = sourceGroups.some(hasTimeGrouping)

  const categoryKeys = new Set<string>()
  for (const group of sourceGroups) {
    if (group.subgroups) {
      for (const subgroup of group.subgroups) {
        categoryKeys.add(subgroup.group_name)
      }
    }
  }

  const chartData: GenericChartDataItem[] | undefined = hasValidTimeGrouping
    ? sourceGroups.map((group) => {
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

  const description = `${sourceGroups[0].group_name} ${showDeposits ? "deposits" : "withdrawals"
    }, by ${sourceGroups[0].groupby_kind} ${sourceGroups[0].subgroups?.length
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
      {hasValidTimeGrouping && chartData ? (
        <GenericBarChart
          data={chartData}
          description={description}
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

export function SankeyBox({
  setCollapsedItems,
  collapsedItems,
}: ValidatedVisualizationProps) {
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
  const [chartHeight, setChartHeight] = useState<number>(0)

  useEffect(() => {
    const updateWidth = () => {
      if (containerRef.current) {
        setChartWidth(containerRef.current.offsetWidth - 30)
        setChartHeight(containerRef.current.offsetHeight - 70)
      }
    }

    updateWidth()
    window.addEventListener("resize", updateWidth)

    return () => {
      window.removeEventListener("resize", updateWidth)
    }
  }, [])

  const COMPONENT_NAME = "Flow Chart"
  const isExpanded = !collapsedItems.includes(COMPONENT_NAME)

  if (error) {
    setCollapsedItems((prev) => [...prev, COMPONENT_NAME])
  }

  const description = "All time transactions grouped by category"

  return (
    <BoxWithText
      text=""
      containerRef={containerRef}
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
      COMPONENT_NAME={COMPONENT_NAME}
      maxH={isExpanded ? 500 : undefined}
      minH={isExpanded ? 500 : undefined}
    >
      <HStack gap={0}>
        <Link to="/sankey-config" href="/sankey-config/">
          <Button
            variant="outline"
            alignSelf="start"
            position="absolute"
            top={2}
            right={2}
            size="sm"
          >
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
          height={chartHeight}
          description={description}
        />
      ) : null}
    </BoxWithText>
  )
}
