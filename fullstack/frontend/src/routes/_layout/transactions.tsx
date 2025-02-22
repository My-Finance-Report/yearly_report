import { useEffect, useState } from "react"

import { Box, Container, Span, Spinner, Text } from "@chakra-ui/react"

import {
  GroupByOption,
  GroupingConfig,
} from "@/components/Common/GroupingConfig"
import { TransactionSourceSelector } from "@/components/Common/TransactionSourceSelector"
import { TransactionsTable } from "@/components/Common/TransactionsTable"
import { VisualizationPanel } from "@/components/Common/VisualizationPanel"
import { WithdrawDepositSelector } from "@/components/Common/WithdrawDepositSelector"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute, Link } from "@tanstack/react-router"
import { type TransactionSourceGroup, TransactionsService } from "../../client"
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

  const [activeTransactionSource, setActiveTransactionSource] =
    useState<TransactionSourceGroup | null>(null)

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
            sourceGroup={activeTransactionSource}
            isLoading={isLoading}
            showDeposits={showDeposits}
          />
          <Box borderWidth={1} padding={2}>
            <WithdrawDepositSelector
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
            />
            <TransactionSourceSelector
              activeTransactionSource={activeTransactionSource}
              setActiveTransactionSource={setActiveTransactionSource}
              allTransactionSources={data.groups}
            />
            <GroupingConfig
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
            />
          </Box>
          <TransactionsTable
            toggleGroup={toggleGroup}
            sourceGroup={activeTransactionSource}
            expandedGroups={expandedGroups}
          />
        </>
      ) : (
        <Link href="/upload-files">
          <Text>
            No transactions found.{" "}
            <Text as="span" textDecoration="underline" color="blue.500">
              Click here
            </Text>{" "}
            to upload files.
          </Text>
        </Link>
      )}
    </Container>
  )
}

export default Transactions
