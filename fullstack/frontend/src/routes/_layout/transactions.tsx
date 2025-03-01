import { useEffect, useState } from "react"

import { Spinner, Text } from "@chakra-ui/react"

import type { CollapsibleName } from "@/components/Common/BoxWithText"
import { FilterGroup } from "@/components/Common/FilterGroup"
import { GroupByOption } from "@/components/Common/GroupingConfig"
import { Legend } from "@/components/Common/Legend"
import { TransactionsTable } from "@/components/Common/TransactionsTable"
import { VisualizationPanel } from "@/components/Common/VisualizationPanel"
import { useQuery } from "@tanstack/react-query"
import { Link, createFileRoute } from "@tanstack/react-router"
import { type TransactionSourceGroup, TransactionsService } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

import { useColorPalette } from "@/hooks/useColor"
import type { TransactionsGetAggregatedTransactionsResponse } from "../../client"

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
})

function Transactions() {
  const [expandedGroups, setExpandedGroups] = useState<{
    [key: string]: boolean
  }>({})
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([
    GroupByOption.month,
    GroupByOption.category,
  ])

  const [showDeposits, setShowDeposits] = useState<boolean>(false)
  const [collapsedItems, setCollapsedItems] = useState<CollapsibleName[]>([])

  const toggleGroup = (sourceId: number, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }))
  }

  const { data, isLoading, error } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", groupingOptions],
    queryFn: () =>
      TransactionsService.getAggregatedTransactions({
        groupBy: groupingOptions,
      }),
    enabled: isLoggedIn(),
  })

  const { getColorForName } = useColorPalette()
  data?.groups.map((group) => {
    group.groups?.map((subgroup) => {
      getColorForName(subgroup.group_name)
      subgroup.subgroups?.map((subsubgroup) => {
        getColorForName(subsubgroup.group_name)
      })
    })
  })

  const [activeTransactionSource, setActiveTransactionSource] =
    useState<TransactionSourceGroup | null>(null)

  useEffect(() => {
    if (data?.groups.length) {
      setActiveTransactionSource(data.groups[0])
    }
  }, [data?.groups])

  const namesForLegends = data?.groups
    .flatMap((group) => group.groups.map((subgroup) => subgroup.subgroups))
    .flatMap((subgroup) =>
      subgroup?.map((subsubgroup) => subsubgroup.group_name),
    )

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        gap: "4px",
        marginBottom: 48,
      }}
    >
      <div>
        <Legend
          toShowNames={namesForLegends}
          collapsedItems={collapsedItems}
          setCollapsedItems={setCollapsedItems}
        />
      </div>
      <div>
        {isLoading ? (
          <Spinner />
        ) : error ? (
          <Text color="red.500">Error loading transactions.</Text>
        ) : data?.groups &&
          data.groups.length > 0 &&
          activeTransactionSource ? (
          <div
            style={{
              flexDirection: "column",
              justifyContent: "center",
              alignItems: "start",
              marginLeft: 20,
            }}
          >
            <FilterGroup
              activeTransactionSource={activeTransactionSource}
              setActiveTransactionSource={setActiveTransactionSource}
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
              data={data}
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />
            <VisualizationPanel
              sourceGroup={activeTransactionSource}
              isLoading={isLoading}
              showDeposits={showDeposits}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />

            <TransactionsTable
              toggleGroup={toggleGroup}
              sourceGroup={activeTransactionSource}
              toShowNames={namesForLegends}
              expandedGroups={expandedGroups}
              showWithdrawals={!showDeposits}
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

export default Transactions
