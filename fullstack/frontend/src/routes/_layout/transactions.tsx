import { useTabs } from "@chakra-ui/react"
import { useEffect, useState } from "react"

import { Container, Spinner, Tabs, Text } from "@chakra-ui/react"

import {
  GroupByOption,
  GroupingConfig,
} from "@/components/Common/GroupingConfig"
import { WithdrawDepositSelector } from '@/components/Common/WithdrawDepositSelector'
import { TransactionSourceSelector } from '@/components/Common/TransactionSourceSelector'
import { TransactionsTable } from "@/components/Common/TransactionsTable"
import { VisualizationPanel } from "@/components/Common/VisualizationPanel"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { TransactionsService, type TransactionSourceGroup } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

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
  const [activeSlice, setActiveSlice] = useState<{
    [sourceId: number]: number
  }>({})
  const [activeGroup, setActiveGroup] = useState<number>(0)

  const toggleGroup = (sourceId: number, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }))
  }

  const tabs = useTabs({
    defaultValue: "0",
    onFocusChange: (value) => setActiveGroup(Number(value.focusedValue)),
  })

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

  const [activeTransactionSource, setActiveTransactionSource] = useState<TransactionSourceGroup | null>(null)

  useEffect(() => {
    if (data?.groups.length) {
      setActiveTransactionSource(data.groups[0])
    }
  }, [data?.groups])



  return (
    <Container maxW="large" py={8}>


      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data?.groups && data.groups.length > 0 && activeTransactionSource ? (
        <>
          <VisualizationPanel
            activeSlice={activeSlice}
            sourceGroup={data?.groups[activeGroup]}
            isLoading={isLoading}
            showDeposits={showDeposits}
          />

          <WithdrawDepositSelector setShowDeposits={setShowDeposits} showDeposits={showDeposits} />
          <TransactionSourceSelector activeTransactionSource={activeTransactionSource} setActiveTransactionSource={setActiveTransactionSource} allTransactionSources={data.groups} />
          <GroupingConfig
            groupingOptions={groupingOptions}
            setGroupingOptions={setGroupingOptions}
          />

          <Tabs.RootProvider variant="enclosed" value={tabs}>
            <Tabs.List>
              {data.groups.map((sourceGroup, index) => (
                <Tabs.Trigger key={index.toString()} value={index.toString()}>
                  {sourceGroup.transaction_source_name}
                </Tabs.Trigger>
              ))}
            </Tabs.List>
            {data.groups.map((sourceGroup, index) => (
              <Tabs.Content key={index.toString()} value={index.toString()}>
                <TransactionsTable
                  toggleGroup={toggleGroup}
                  sourceGroup={sourceGroup}
                  setActiveSlice={setActiveSlice}
                  expandedGroups={expandedGroups}
                />
              </Tabs.Content>
            ))}
          </Tabs.RootProvider>
        </>

      ) : (
        <Text>No transactions found.</Text>
      )}
    </Container>
  )
}

export default Transactions
