import { ChevronDownIcon, ChevronRightIcon } from "@chakra-ui/icons"
import {
  Box,
  Collapsible,
  HStack,
  Table,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRow,
} from "@chakra-ui/react"
import React from "react"

import { useColorPalette } from "@/hooks/useColor"
import { FiEdit } from "react-icons/fi"
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
}: {
  sourceGroup: TransactionSourceGroup
  toggleGroup: (sourceId: number, groupKey: string) => void
  toShowNames?: (string | undefined)[] | undefined
  expandedGroups: { [key: string]: boolean }
}) {
  return (
    <Table.Root variant="outline" borderRadius="md">
      <TableHeader>
        <TableRow>
          <TableColumnHeader />
          <TableColumnHeader>
            {sourceGroup.groups[0].groupby_kind?.toLocaleUpperCase()}
          </TableColumnHeader>
          <TableColumnHeader textAlign="end">WITHDRAWALS</TableColumnHeader>
          <TableColumnHeader textAlign="end">DEPOSITS</TableColumnHeader>
          <TableColumnHeader textAlign="end">BALANCE</TableColumnHeader>
        </TableRow>
      </TableHeader>
      <TableBody>
        {renderGroups({
          groups: sourceGroup.groups,
          sourceId: sourceGroup.transaction_source_id,
          pathPrefix: "",
          toShowNames,
          toggleGroup,
          expandedGroups,
        })}
        <TableRow fontWeight="bold">
          <TableCell colSpan={2}>Source Totals</TableCell>
          <TableCell textAlign="end">{sourceGroup.total_withdrawals}</TableCell>
          <TableCell textAlign="end">{sourceGroup.total_deposits}</TableCell>
          <TableCell textAlign="end">{sourceGroup.total_balance}</TableCell>
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
}: {
  groups: AggregatedGroup[]
  sourceId: number
  pathPrefix: string
  toggleGroup: (sourceId: number, groupKey: string) => void
  expandedGroups: { [key: string]: boolean }
  toShowNames?: (string | undefined)[] | undefined
}) {
  return groups.map((group, idx) => {
    const groupKey = pathPrefix
      ? `${pathPrefix}-${group.group_id}`
      : `${group.group_id}`
    const isExpanded = expandedGroups[`${sourceId}-${groupKey}`] || false

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
          <TableCell textAlign="end">{formatAmount(group.total_withdrawals)}</TableCell>
          <TableCell textAlign="end">{formatAmount(group.total_deposits)}</TableCell>
          <TableCell textAlign="end">{formatAmount(group.total_balance)}</TableCell>
        </TableRow>

        {/* Expanded Content */}
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
                            WITHDRAWALS
                          </TableColumnHeader>
                          <TableColumnHeader textAlign="end">
                            DEPOSITS
                          </TableColumnHeader>
                          <TableColumnHeader textAlign="end">
                            BALANCE
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
  const handleEdit = () => {
    // Logic to open the modal goes here
  }
  return (
    <TableRow>
      <TableCell>{transaction.description}</TableCell>
      <TableCell>
        {new Date(transaction.date_of_transaction).toLocaleDateString()}
      </TableCell>
      <TableCell>{formatAmount(transaction.amount)}</TableCell>
      <TableCell>{transaction.kind}</TableCell>
    </TableRow>
  )
}


function formatAmount(amount: number) {
  return amount.toLocaleString("en-US", {
    style: "currency",
    currency: "USD",
  })
}