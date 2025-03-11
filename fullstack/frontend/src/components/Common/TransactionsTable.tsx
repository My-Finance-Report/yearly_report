import { ChevronDownIcon, ChevronRightIcon } from "@chakra-ui/icons"
import {
  Box,
  Button,
  Collapsible,
  HStack,
  Progress,
  Table,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRow,
  useDisclosure,
} from "@chakra-ui/react"
import React, { useMemo } from "react"

import { useColorPalette } from "@/hooks/useColor"
import { FiEdit } from "react-icons/fi"
import type {
  AggregatedGroup,
  TransactionOut,
  TransactionsGetAggregatedTransactionsResponse,
} from "../../client"
import EditTransaction from "./EditTransaction"

export function TransactionsTable({
  data,
  toggleGroup,
  expandedGroups,
  toShowNames,
  showWithdrawals,
  isMobile,
}: {
  data:TransactionsGetAggregatedTransactionsResponse
  toggleGroup: (sourceId: number | string, groupKey: string) => void
  toShowNames?: (string | undefined)[] | undefined
  expandedGroups: { [key: string]: boolean }
  showWithdrawals: boolean
  isMobile: boolean
}) {




  return (
    <Table.Root variant="outline" borderRadius="md">
      <TableHeader>
        <TableRow>
          <TableColumnHeader />
          <TableColumnHeader>
            {data.groups[0].groupby_kind?.toLocaleUpperCase()}
          </TableColumnHeader>
          <TableColumnHeader textAlign="end">EXPENSE</TableColumnHeader>
          <TableColumnHeader textAlign="end">DEPOSIT</TableColumnHeader>
          {!isMobile && (
            <TableColumnHeader textAlign="end">TOTAL</TableColumnHeader>
          )}
        </TableRow>
      </TableHeader>
      <TableBody>
        <MemoizedRenderGroups
          groups={data.groups}
          totalWidthdrawals={data.overall_withdrawals}
          totalDeposits={data.overall_deposits}
          isMobile={!!isMobile} 
          showWithdrawals={showWithdrawals}
          pathPrefix=""
          toShowNames={toShowNames}
          toggleGroup={toggleGroup}
          expandedGroups={expandedGroups}
        />
        <TableRow fontWeight="bold">
          <TableCell colSpan={2}>Source Totals</TableCell>
          <TableCell textAlign="end">
            {formatAmount(data.overall_withdrawals)}
          </TableCell>
          <TableCell textAlign="end">
            {formatAmount(data.overall_deposits)}
          </TableCell>
          {!isMobile && (
          <TableCell textAlign="end">
            {formatAmount(data.overall_balance)}
          </TableCell>
          )}
        </TableRow>
      </TableBody>
    </Table.Root>
  )
}

const MemoizedRenderGroups = React.memo(function RenderGroups({
  groups,
  pathPrefix,
  expandedGroups,
  isMobile,
  toggleGroup,
  toShowNames,
  totalWidthdrawals,
  totalDeposits,
  showWithdrawals,
}: {
  groups: AggregatedGroup[]
  pathPrefix: string
  toggleGroup: (groupId: number | string, groupKey: string) => void
  expandedGroups: { [key: string]: boolean }
  toShowNames?: (string | undefined)[] | undefined
  totalWidthdrawals?: number
  showWithdrawals: boolean
  isMobile: boolean
  totalDeposits?: number
}) {
  const memoizedGroups = useMemo(() => {
    return groups.map((group) => {
      const groupKey = pathPrefix
        ? `${pathPrefix}-${group.group_id}`
        : `${group.group_id}`

      const isExpanded = expandedGroups[`${group.group_id}-${groupKey}`] || false

      const totalAmount = showWithdrawals ? totalWidthdrawals : totalDeposits
      const specificAmount = showWithdrawals
        ? group.total_withdrawals
        : group.total_deposits

      const { getColorForName } = useColorPalette()


      return (
        <React.Fragment key={groupKey}>
          <TableRow
            style={{ cursor: "pointer" }}
            onClick={() => toggleGroup(group.group_id,groupKey)}
          >
            <TableCell>
              {isExpanded ? <ChevronDownIcon /> : <ChevronRightIcon />}
            </TableCell>
            <TableCell>
              <HStack justify={"space-between"}>
                <HStack>
                  {toShowNames?.includes(group.group_name) && (
                    <Box
                      width="16px"
                      borderWidth={1}
                      padding={3}
                      height="16px"
                      borderRadius="50%"
                      backgroundColor={getColorForName(group.group_name)}
                    />
                  )}
                  {group.group_name}
                </HStack>
                {totalAmount && (
                  <PercentageBar amount={specificAmount} total={totalAmount} isMobile={isMobile} />
                )}
              </HStack>
            </TableCell>
            <TableCell textAlign="end">
              {formatAmount(group.total_withdrawals)}
            </TableCell>
            <TableCell textAlign="end">
              {formatAmount(group.total_deposits)}
            </TableCell>
            {!isMobile && (
            <TableCell textAlign="end">
              {formatAmount(group.total_balance)}
            </TableCell>
            )}
          </TableRow>

          <Collapsible.Root open={isExpanded} lazyMount asChild>
            <TableRow>
              <TableCell colSpan={5} p={0}>
                <Collapsible.Content>
                  <Box pl={isMobile ? 2 : 4}>
                    {group.subgroups && group.subgroups.length > 0 ? (
                      <Table.Root variant="outline" size="sm">
                        <TableHeader>
                          <TableRow>
                            <TableColumnHeader />
                            <TableColumnHeader>
                              {group.subgroups[0].groupby_kind?.toLocaleUpperCase()}
                            </TableColumnHeader>
                            <TableColumnHeader textAlign="end">
                              EXPENSE
                            </TableColumnHeader>
                            <TableColumnHeader textAlign="end">
                              DEPOSIT
                            </TableColumnHeader>
                            {!isMobile && (
                              <TableColumnHeader textAlign="end">
                                TOTAL
                              </TableColumnHeader>
                            )}
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          <MemoizedRenderGroups
                            groups={group.subgroups}
                            pathPrefix={groupKey}
                            toggleGroup={toggleGroup}
                            isMobile={isMobile}
                            expandedGroups={expandedGroups}
                            toShowNames={toShowNames}
                            showWithdrawals={showWithdrawals}
                          />
                        </TableBody>
                      </Table.Root>
                    ) : (
                      <Table.Root variant="outline" size="sm">
                        <TableHeader>
                          <TableRow>
                            <TableColumnHeader>DESCRIPTION</TableColumnHeader>
                            {!isMobile && (
                            <TableColumnHeader>DATE</TableColumnHeader>
                            )}
                            <TableColumnHeader>AMOUNT</TableColumnHeader>
                            <TableColumnHeader>KIND</TableColumnHeader>
                            <TableColumnHeader />
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          {group.transactions?.map((transaction, index) => (
                            <TransactionRow key={index.toString()} transaction={transaction}  isMobile={isMobile}/>
                          ))}
                        </TableBody>
                      </Table.Root>
                    )}
                  </Box>
                </Collapsible.Content>
              </TableCell>
            </TableRow>
          </Collapsible.Root>
        </React.Fragment>
      )
    })
  }, [
    groups,
    pathPrefix,
    expandedGroups,
    toggleGroup,
    toShowNames,
    totalWidthdrawals,
    totalDeposits,
    showWithdrawals,
  ])

  return <>{memoizedGroups}</>
})

export default MemoizedRenderGroups

function TransactionRow({ transaction, isMobile }: { transaction: TransactionOut, isMobile: boolean }) {
  const editTransactionModal = useDisclosure()

  return (
    <TableRow>
      <TableCell>{transaction.description}</TableCell>
      {!isMobile && (
      <TableCell>
        {new Date(transaction.date_of_transaction).toLocaleDateString()}
      </TableCell>
      )}
      <TableCell>{formatAmount(transaction.amount)}</TableCell>
      <TableCell>
        {"withdrawal" === transaction.kind ? "Expense" : "Deposit"}
      </TableCell>
      <TableCell>
        <Button
          onClick={editTransactionModal.onOpen}
          size="sm"
          variant="outline"
        >
          <FiEdit size="8px" />
        </Button>
      </TableCell>
      <EditTransaction
        transaction={transaction}
        isOpen={editTransactionModal.open}
        onClose={editTransactionModal.onClose}
      />
    </TableRow>
  )
}

function formatAmount(amount: number) {
  return amount.toLocaleString("en-US", {
    style: "currency",
    currency: "USD",
  })
}

function PercentageBar({ amount, total,isMobile }: { amount: number; total: number; isMobile: boolean }) {

  if (isMobile) {
    return null
  }
  return (
    <Progress.Root
      defaultValue={(Math.abs(amount) / Math.abs(total)) * 100}
      maxW="sm"
    >
      <HStack gap="5" minW={200}>
        <Progress.Track maxW={150} minW={150} flex="1">
          <Progress.Range />
        </Progress.Track>
      </HStack>
    </Progress.Root>
  )
}
