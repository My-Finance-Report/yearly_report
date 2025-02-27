import { useEffect, useState } from "react"

import { Container, Spinner, Text } from "@chakra-ui/react"

import BoxWithText from "@/components/Common/BoxWithText"
import {
  GroupByOption,
  GroupingConfig,
} from "@/components/Common/GroupingConfig"
import { Legend } from "@/components/Common/Legend"
import { TransactionSourceSelector } from "@/components/Common/TransactionSourceSelector"
import { TransactionsTable } from "@/components/Common/TransactionsTable"
import { VisualizationPanel } from "@/components/Common/VisualizationPanel"
import { WithdrawDepositSelector } from "@/components/Common/WithdrawDepositSelector"
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
  const [isExpanded, setIsExpanded] = useState(true)

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
    <Container maxW="large" py={8}>
      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data?.groups && data.groups.length > 0 && activeTransactionSource ? (
        <>
          <Legend toShowNames={namesForLegends} />
          <VisualizationPanel
            sourceGroup={activeTransactionSource}
            isLoading={isLoading}
            showDeposits={showDeposits}
          />
          <BoxWithText
            text="Filters"
            isExpanded={isExpanded}
            setIsExpanded={setIsExpanded}
          >
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
          </BoxWithText>
          <TransactionsTable
            toggleGroup={toggleGroup}
            sourceGroup={activeTransactionSource}
            toShowNames={namesForLegends}
            expandedGroups={expandedGroups}
          />
        </>
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
    </Container>
  )
}

export default Transactions
