import { ChevronDownIcon, ChevronRightIcon } from "@chakra-ui/icons"
import {
  Box,
  Button,
  Collapsible,
  HStack,
  Table,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRow,
  Text,
  useDisclosure,
} from "@chakra-ui/react"
import React, { useState } from "react"

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
  toShowNames,
  showWithdrawals,
  isMobile,
}: {
  data: TransactionsGetAggregatedTransactionsResponse
  toShowNames?: (string | undefined)[] | undefined
  showWithdrawals: boolean
  isMobile: boolean
}) {
  const [expandedGroups, setExpandedGroups] = useState<{
    [key: string]: boolean
  }>({})

  const toggleGroup = (sourceId: number | string, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }))
  }

  if (!data.groups.length) {
    return null
  }

  const isBudget = data.groups[0].groupby_kind === "budget"

  return (
    <Table.Root variant="outline" borderRadius="md">
      <TableHeader>
        <TableRow>
          <TableColumnHeader />
          <TableColumnHeader>
            {data.groups[0].groupby_kind?.toLocaleUpperCase()}
          </TableColumnHeader>
          <TableColumnHeader>
            {isMobile ? null : isBudget ? "BUDGET" : ""}
          </TableColumnHeader>
          {isMobile ? (
            <TableColumnHeader textAlign="end" colSpan={1}>
              EXPENSE / DEPOSIT
            </TableColumnHeader>
          ) : (
            <>
              <TableColumnHeader textAlign="end">EXPENSE</TableColumnHeader>
              <TableColumnHeader textAlign="end">DEPOSIT</TableColumnHeader>
            </>
          )}
          {!isMobile && (
            <TableColumnHeader textAlign="end">TOTAL</TableColumnHeader>
          )}
        </TableRow>
      </TableHeader>
      <TableBody>
        <RenderGroups
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
          <TableCell colSpan={2}>Totals</TableCell>
          <TableCell />
          {isMobile ? (
            <TableCell textAlign="end">
              {formatAmount(data.overall_withdrawals)} /{" "}
              {formatAmount(data.overall_deposits)}
            </TableCell>
          ) : (
            <>
              <TableCell textAlign="end">
                {formatAmount(data.overall_withdrawals)}
              </TableCell>
              <TableCell textAlign="end">
                {formatAmount(data.overall_deposits)}
              </TableCell>
              <TableCell textAlign="end">
                {formatAmount(data.overall_balance)}
              </TableCell>
            </>
          )}
        </TableRow>
      </TableBody>
    </Table.Root>
  )
}

function RenderGroups({
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
  return (
    <>
      {groups.map((group) => {
        const groupKey = pathPrefix
          ? `${pathPrefix}-${group.group_id}`
          : `${group.group_id}`

        const isExpanded =
          expandedGroups[`${group.group_id}-${groupKey}`] || false

        const totalAmount = showWithdrawals ? totalWidthdrawals : totalDeposits
        const budgetedTotal = group.budgeted_total
        const specificAmount = showWithdrawals
          ? group.total_withdrawals
          : group.total_deposits

        const { getColorForName } = useColorPalette()

        const isUnbudgeted = group.group_name === "Unbudgeted"

        return (
          <React.Fragment key={groupKey}>
            <TableRow
              style={{ cursor: "pointer" }}
              onClick={() => toggleGroup(group.group_id, groupKey)}
            >
              <TableCell>
                {isExpanded ? <ChevronDownIcon /> : <ChevronRightIcon />}
              </TableCell>
              <TableCell>
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
              </TableCell>
              {totalAmount || budgetedTotal ? (
                <TableCell>
                  <PercentageBar
                    amount={specificAmount}
                    total={totalAmount}
                    budgetedTotal={budgetedTotal}
                    isUnbudgeted={isUnbudgeted}
                    isMobile={isMobile}
                  />
                </TableCell>
              ) : (
                <TableCell />
              )}
              {isMobile ? (
                <TableCell textAlign="end">
                  {formatAmount(group.total_withdrawals)} /{" "}
                  {formatAmount(group.total_deposits)}
                </TableCell>
              ) : (
                <>
                  <TableCell textAlign="end">
                    {formatAmount(group.total_withdrawals)}
                  </TableCell>
                  <TableCell textAlign="end">
                    {formatAmount(group.total_deposits)}
                  </TableCell>
                  <TableCell textAlign="end">
                    {formatAmount(group.total_balance)}
                  </TableCell>
                </>
              )}
            </TableRow>

            <Collapsible.Root open={isExpanded} lazyMount asChild>
              <TableRow>
                <TableCell colSpan={6} p={0}>
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
                              {group.budgeted_total ? (
                                <TableColumnHeader>BUDGET</TableColumnHeader>
                              ) : (
                                <TableColumnHeader />
                              )}
                              {isMobile ? (
                                <TableColumnHeader textAlign="end" colSpan={1}>
                                  EXPENSE / DEPOSIT
                                </TableColumnHeader>
                              ) : (
                                <>
                                  <TableColumnHeader textAlign="end">
                                    EXPENSE
                                  </TableColumnHeader>
                                  <TableColumnHeader textAlign="end">
                                    DEPOSIT
                                  </TableColumnHeader>
                                  <TableColumnHeader textAlign="end">
                                    TOTAL
                                  </TableColumnHeader>
                                </>
                              )}
                            </TableRow>
                          </TableHeader>
                          <TableBody>
                            <RenderGroups
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
                              <TransactionRow
                                key={index.toString()}
                                transaction={transaction}
                                isMobile={isMobile}
                              />
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
      })}
    </>
  )
}

function TransactionRow({
  transaction,
  isMobile,
}: {
  transaction: TransactionOut
  isMobile: boolean
}) {
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

function PercentageBar({
  amount,
  total,
  budgetedTotal,
  isUnbudgeted,
  isMobile,
}: {
  amount: number
  total: number | undefined
  budgetedTotal?: number | undefined
  isUnbudgeted: boolean
  isMobile: boolean
}) {
  if (isMobile) {
    return null
  }

  const totalToUse = budgetedTotal ? budgetedTotal : total

  if (!totalToUse) {
    return null
  }

  const value = (Math.abs(amount) / Math.abs(totalToUse)) * 100

  if (isUnbudgeted) {
    return <Text>n/a</Text>
  }

  if (budgetedTotal) {
    return (
      <Text color={value > 100 ? "red" : "green"}>
        {value.toFixed()}% (${totalToUse} was budgeted)
      </Text>
    )
  }
  return null

  /*   return (
    <Progress.Root value={value} maxW="sm">
      <HStack gap="5" minW={200}>
        <Progress.Track maxW={150} minW={150} flex="1">
          <Progress.Range />
        </Progress.Track>
      </HStack>
    </Progress.Root>
  );
 */
}
