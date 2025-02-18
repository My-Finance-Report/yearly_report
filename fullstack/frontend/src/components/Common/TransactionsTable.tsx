import React from "react";
import { ChevronRightIcon, ChevronDownIcon } from "@chakra-ui/icons";
import {
  Table,
  TableHeader,
  TableBody,
  TableRow,
  TableColumnHeader,
  TableCell,
  Box,
  Collapsible,
} from "@chakra-ui/react";

import type { AggregatedGroup, TransactionSourceGroup } from "../../client";

export function TransactionsTable({
  sourceGroup,
  toggleGroup,
  expandedGroups,
  setActiveSlice,
}: {
  sourceGroup: TransactionSourceGroup;
  toggleGroup: (sourceId: number, groupKey: string) => void;
  setActiveSlice: React.Dispatch<
    React.SetStateAction<{ [sourceId: number]: number }>
  >;
  expandedGroups: { [key: string]: boolean };
}) {
  return (
    <Table.Root variant="outline">
      <TableHeader>
        <TableRow>
          <TableColumnHeader></TableColumnHeader>
          <TableColumnHeader>Group</TableColumnHeader>
          <TableColumnHeader textAlign="end">Withdrawals</TableColumnHeader>
          <TableColumnHeader textAlign="end">Deposits</TableColumnHeader>
          <TableColumnHeader textAlign="end">Balance</TableColumnHeader>
        </TableRow>
      </TableHeader>
      <TableBody>
        {renderGroups({
          groups: sourceGroup.groups,
          sourceId: sourceGroup.transaction_source_id,
          pathPrefix: "",
          toggleGroup,
          setActiveSlice,
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
  );
}

function renderGroups({
  groups,
  sourceId,
  pathPrefix,
  expandedGroups,
  toggleGroup,
  setActiveSlice,
}: {
  groups: AggregatedGroup[];
  sourceId: number;
  pathPrefix: string;
  toggleGroup: (sourceId: number, groupKey: string) => void;
  setActiveSlice: React.Dispatch<
    React.SetStateAction<{ [sourceId: number]: number }>
  >;
  expandedGroups: { [key: string]: boolean };
}) {
  return groups.map((group, idx) => {
    const groupKey = pathPrefix
      ? `${pathPrefix}-${group.group_id}`
      : `${group.group_id}`;
    const isExpanded = expandedGroups[`${sourceId}-${groupKey}`] || false;

    return (
      <React.Fragment key={groupKey}>
        {/* Expandable row */}
        <TableRow
          style={{ cursor: "pointer" }}
          onMouseEnter={() => {
            if (!pathPrefix) {
              setActiveSlice((prev) => ({ ...prev, [sourceId]: idx }));
            }
          }}
          onMouseLeave={() => {
            if (!pathPrefix) {
              setActiveSlice((prev) => ({ ...prev, [sourceId]: 0 }));
            }
          }}
          onClick={() => toggleGroup(sourceId, groupKey)}
        >
          <TableCell>
            {isExpanded ? <ChevronDownIcon /> : <ChevronRightIcon />}
          </TableCell>
          <TableCell>{group.group_name}</TableCell>
          <TableCell textAlign="end">{group.total_withdrawals}</TableCell>
          <TableCell textAlign="end">{group.total_deposits}</TableCell>
          <TableCell textAlign="end">{group.total_balance}</TableCell>
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
                          <TableColumnHeader></TableColumnHeader>
                          <TableColumnHeader>Group</TableColumnHeader>
                          <TableColumnHeader textAlign="end">Withdrawals</TableColumnHeader>
                          <TableColumnHeader textAlign="end">Deposits</TableColumnHeader>
                          <TableColumnHeader textAlign="end">Balance</TableColumnHeader>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        {renderGroups({
                          groups: group.subgroups,
                          sourceId,
                          pathPrefix: groupKey,
                          toggleGroup,
                          setActiveSlice,
                          expandedGroups,
                        })}
                      </TableBody>
                    </Table.Root>
                  ) : (
                    <Table.Root variant="outline" size="sm">
                      <TableHeader>
                        <TableRow>
                          <TableColumnHeader>ID</TableColumnHeader>
                          <TableColumnHeader>Description</TableColumnHeader>
                          <TableColumnHeader>Date</TableColumnHeader>
                          <TableColumnHeader textAlign="end">Amount</TableColumnHeader>
                          <TableColumnHeader>Kind</TableColumnHeader>
                          <TableColumnHeader>Archived</TableColumnHeader>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        {group.transactions?.map((transaction) => (
                          <TableRow key={transaction.id}>
                            <TableCell>{transaction.id}</TableCell>
                            <TableCell>{transaction.description}</TableCell>
                            <TableCell>
                              {new Date(transaction.date_of_transaction).toLocaleDateString()}
                            </TableCell>
                            <TableCell textAlign="end">{transaction.amount}</TableCell>
                            <TableCell>{transaction.kind}</TableCell>
                            <TableCell>{transaction.archived ? "Yes" : "No"}</TableCell>
                          </TableRow>
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
    );
  });
}
