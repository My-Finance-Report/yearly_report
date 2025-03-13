import { useEffect, useState } from "react"

import { Spinner, Text, Flex } from "@chakra-ui/react"

import type { CollapsibleName } from "@/components/Common/BoxWithText"
import { FilterGroup, FilterInfo } from "@/components/Common/FilterGroup"
import { GroupByOption } from "@/components/Common/GroupingConfig"
import { Legend } from "@/components/Common/Legend"
import { TransactionsTable } from "@/components/Common/TransactionsTable"
import { VisualizationPanel } from "@/components/Common/VisualizationPanel"
import { useQuery } from "@tanstack/react-query"
import { Link, createFileRoute } from "@tanstack/react-router"
import { AggregatedGroup, DemoService } from "../client"

import { useColorPalette } from "@/hooks/useColor"
import type { TransactionsGetAggregatedTransactionsResponse } from "../client"
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import { useIsMobile } from "@/hooks/useIsMobile"

export const Route = createFileRoute("/demo")({
  component: DemoLayout,
})

function Demo() {
  const [expandedGroups, setExpandedGroups] = useState<{
    [key: string]: boolean
  }>({})
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([
    GroupByOption.month,
    GroupByOption.category,
    GroupByOption.account,
  ])

  const [accounts, setAccounts] = useState<string[] | null>(null)
  const [categories, setCategories] = useState<string[] | null>(null)
  const [months, setMonths] = useState<string[] | null>(null)
  const [years, setYears] = useState<string[] | null>(null)
  const [budgets, setBudgets] = useState<string[] | null>(null)

  const filterInfo: FilterInfo = {
    budgets,
    years,
    accounts,
    months,
    categories,
    setBudgets,
    setYears,
    setAccounts,
    setMonths,
    setCategories,
  }

  const [showDeposits, setShowDeposits] = useState<boolean>(false)
  const [collapsedItems, setCollapsedItems] = useState<CollapsibleName[]>([])

  const toggleGroup = (sourceId: number | string, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }))
  }

  const { data, isLoading, error, refetch } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["demo_aggregatedTransactions", groupingOptions],
    queryFn: () =>
      DemoService.getDemoAggregatedTransactions({
        groupBy: groupingOptions,
        years,
        accounts,
        months,
        categories,
      }),
  })

  useEffect(() => {
    refetch()
  }, [filterInfo])

  const isMobile = useIsMobile()

  const { getColorForName } = useColorPalette()
  data?.groups.map((group) => {
      getColorForName(group.group_name)
      group.subgroups?.map((subgroup) => {
        getColorForName(subgroup.group_name)
      })
  })

  const [activeGrouping, setactiveGrouping] =
    useState<AggregatedGroup[] | null>(null)

  useEffect(() => {
    if(data?.grouping_options_choices) {
      setYears(data.grouping_options_choices[GroupByOption.year])
      setMonths(data.grouping_options_choices[GroupByOption.month])
      setCategories(data.grouping_options_choices[GroupByOption.category])
      setAccounts(data.grouping_options_choices[GroupByOption.account])
    }
  },[])

  useEffect(() => {
    if (data?.groups.length) {
      setactiveGrouping(data.groups)
    }
    
  }, [data?.groups])

  const namesForLegends = data?.groups
    .flatMap((group) => group?.subgroups?.map((subgroup) => subgroup.group_name))

  if (isLoading) {
    return <Spinner />
  }

  if (error) {
    return <Text color="red.500">Error loading transactions.</Text>
  }



  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        gap: "4px",
        marginBottom: 48,
      }}
    >

      <Legend
        collapsedItems={collapsedItems}
        setCollapsedItems={setCollapsedItems}
      />
      <div>
        {data?.groups && data.groups.length > 0 && activeGrouping ? (
          <div
            style={{
              flexDirection: "column",
              justifyContent: "center",
              alignItems: "start",
            }}
          >
            <FilterGroup
              setShowDeposits={setShowDeposits}
              filterInfo={filterInfo}
              groupingOptionsChoices={data.grouping_options_choices as { [key in GroupByOption]: string[] }}
              showDeposits={showDeposits}
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />
            <VisualizationPanel
              sourceGroups={activeGrouping}
              isLoading={isLoading}
              showDeposits={showDeposits}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />

            <TransactionsTable
              toggleGroup={toggleGroup}
              data={data}
              toShowNames={namesForLegends}
              expandedGroups={expandedGroups}
              showWithdrawals={!showDeposits}
              isMobile={isMobile}
            />
          </div>
        ) : (
          <Link to="/upload-files" href="/upload-files">
            <Text>
              No transactions found.{" "}
              <Text as="span" textDecoration="underline" color="blue.500">
                Click here
              </Text>{" "}
              to upload files.
            </Text>
          </Link>
        )}
      </div>
    </div>
  )
}


export default function DemoLayout() {

  return (
    <div style={{ backgroundColor: "background", padding: 20, minHeight: "100vh" }}>
      <SegmentedNavigation />
        <Flex justify="center" align="center" width="full">
          <Demo/>
        </Flex>
    </div>
  )
}

