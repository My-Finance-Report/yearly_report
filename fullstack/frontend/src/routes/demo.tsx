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
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([
    GroupByOption.month,
    GroupByOption.category,
    GroupByOption.account,
  ])

  const [accounts, setAccounts] = useState<string[]>([])
  const [categories, setCategories] = useState<string[]>([])
  const [months, setMonths] = useState<string[]>([])
  const [years, setYears] = useState<string[]>([])
  const [budgets, setBudgets] = useState<string[]>([])

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
              data={data}
              toShowNames={namesForLegends}
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
    <div style={{ backgroundColor: "background",  minHeight: "100vh" }}>
      <SegmentedNavigation />
        <Flex justify="center" align="center" width="full">
          <Demo/>
        </Flex>
    </div>
  )
}

