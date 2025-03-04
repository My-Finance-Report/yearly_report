import { ChevronDownIcon, ChevronRightIcon } from "@chakra-ui/icons"
import {
  Box,
  Button,
  Progress,
  Collapsible,
  HStack,
  Table,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRow,
  useDisclosure,
} from "@chakra-ui/react"
import React from "react"

import { useColorPalette } from "@/hooks/useColor"
import { FiEdit } from "react-icons/fi"
import EditTransaction from "./EditTransaction"
import type {
  AggregatedGroup,
  TransactionOut,
  TransactionSourceGroup,
} from "../../client"

export function TransactionsTable({
  sourceGroup,
  toggleGroup,
  expandedGroups,
  toShowNames,
  showWithdrawals,
}: {
  sourceGroup: TransactionSourceGroup
  toggleGroup: (sourceId: number, groupKey: string) => void
  toShowNames?: (string | undefined)[] | undefined
  expandedGroups: { [key: string]: boolean }
  showWithdrawals: boolean
}) {
  return (
    <Table.Root variant="outline" borderRadius="md">
      <TableHeader>
        <TableRow>
          <TableColumnHeader />
          <TableColumnHeader>
            {sourceGroup.groups[0].groupby_kind?.toLocaleUpperCase()}
          </TableColumnHeader>
          <TableColumnHeader textAlign="end">EXPENSE</TableColumnHeader>
          <TableColumnHeader textAlign="end">DEPOSIT</TableColumnHeader>
          <TableColumnHeader textAlign="end">TOTAL</TableColumnHeader>
        </TableRow>
      </TableHeader>
      <TableBody>
        {renderGroups({
          groups: sourceGroup.groups,
          sourceId: sourceGroup.transaction_source_id,
          totalWidthdrawals: sourceGroup.total_withdrawals,
          totalDeposits: sourceGroup.total_deposits,
          showWithdrawals,
          pathPrefix: "",
          toShowNames,
          toggleGroup,
          expandedGroups,
        })}
        <TableRow fontWeight="bold">
          <TableCell colSpan={2}>Source Totals</TableCell>
          <TableCell textAlign="end">{formatAmount(sourceGroup.total_withdrawals)}</TableCell>
          <TableCell textAlign="end">{formatAmount(sourceGroup.total_deposits)}</TableCell>
          <TableCell textAlign="end">{formatAmount(sourceGroup.total_balance)}</TableCell>
        </TableRow>
      </TableBody>
    </Table.Root>
  )
}

function renderGroups({
  groups,
  sourceId,
  pathPrefix,
  expandedGroups,
  toggleGroup,
  toShowNames,
  totalWidthdrawals,
  totalDeposits,
  showWithdrawals,
}: {
  groups: AggregatedGroup[]
  sourceId: number
  pathPrefix: string
  toggleGroup: (sourceId: number, groupKey: string) => void
  expandedGroups: { [key: string]: boolean }
  toShowNames?: (string | undefined)[] | undefined
  totalWidthdrawals?: number
  showWithdrawals: boolean
  totalDeposits?: number
}) {
  return groups.map((group, idx) => {
    const groupKey = pathPrefix
      ? `${pathPrefix}-${group.group_id}`
      : `${group.group_id}`

    const isExpanded = expandedGroups[`${sourceId}-${groupKey}`] || false

    const totalAmount = showWithdrawals ? totalWidthdrawals : totalDeposits
    const specificAmount = showWithdrawals ? group.total_withdrawals : group.total_deposits


    const { getColorForName } = useColorPalette()

    return (
      <React.Fragment key={groupKey}>
        <TableRow
          style={{ cursor: "pointer" }}
          onClick={() => toggleGroup(sourceId, groupKey)}
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
              {totalAmount &&
              <PercentageBar amount={specificAmount} total={totalAmount} />
  }
            </HStack>
          </TableCell>
          <TableCell textAlign="end">{formatAmount(group.total_withdrawals)}</TableCell>
          <TableCell textAlign="end">{formatAmount(group.total_deposits)}</TableCell>
          <TableCell textAlign="end">{formatAmount(group.total_balance)}</TableCell>
        </TableRow>

        <Collapsible.Root open={isExpanded} asChild unmountOnExit>
          <TableRow>
            <TableCell colSpan={5} p={0}>
              <Collapsible.Content>
                <Box pl={4}>
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
                          <TableColumnHeader textAlign="end">
                            TOTAL
                          </TableColumnHeader>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        {renderGroups({
                          groups: group.subgroups,
                          sourceId,
                          pathPrefix: groupKey,
                          toggleGroup,
                          expandedGroups,
                          toShowNames,
                          showWithdrawals,
                        })}
                      </TableBody>
                    </Table.Root>
                  ) : (
                    <Table.Root variant="outline" size="sm">
                      <TableHeader>
                        <TableRow>
                          <TableColumnHeader>DESCRIPTION</TableColumnHeader>
                          <TableColumnHeader>DATE</TableColumnHeader>
                          <TableColumnHeader>AMOUNT</TableColumnHeader>
                          <TableColumnHeader>KIND</TableColumnHeader>
                          <TableColumnHeader></TableColumnHeader>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        {group.transactions?.map((transaction) => (
                          <TransactionRow transaction={transaction} />
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
}

function TransactionRow({ transaction }: { transaction: TransactionOut }) {

  const editTransactionModal = useDisclosure()

  return (
    <TableRow>
      <TableCell>{transaction.description}</TableCell>
      <TableCell>
        {new Date(transaction.date_of_transaction).toLocaleDateString()}
      </TableCell>
      <TableCell>{formatAmount(transaction.amount)}</TableCell>
      <TableCell>{'withdrawal' === transaction.kind ? 'Expense' : 'Deposit'}</TableCell>
      <TableCell><Button onClick={editTransactionModal.onOpen} size="sm" variant="outline"><FiEdit size="8px" /></Button></TableCell>
      <EditTransaction transaction={transaction} isOpen={editTransactionModal.open} onClose={editTransactionModal.onClose} />
    </TableRow>
    
  )
}


function formatAmount(amount: number) {
  return amount.toLocaleString("en-US", {
    style: "currency",
    currency: "USD",
  })
}

function PercentageBar({ amount, total }: { amount: number; total: number }) {
  return (
    <Progress.Root defaultValue={Math.abs(amount) / Math.abs(total) * 100} maxW="sm">
      <HStack gap="5" minW={200}>
        <Progress.Track maxW={150} minW={150} flex="1">
          <Progress.Range />
        </Progress.Track>
      </HStack>
    </Progress.Root>
  )
}