"use client";

import React, { useState } from "react";
import {
  Table,
  TableHeader,
  TableBody,
  TableRow,
  TableColumnHeader,
  TableCell,
  Collapsible,
  useTabs
} from "@chakra-ui/react";
import { ChevronRightIcon, ChevronDownIcon } from "@chakra-ui/icons";

import {
  Container,
  Heading,
  Box,
  Flex,
  Spinner,
  Text,
  Tabs,
  ButtonGroup,
  Button,
} from "@chakra-ui/react";
import { createFileRoute } from "@tanstack/react-router";
import { useQuery } from "@tanstack/react-query";
import { TransactionsService } from "../../client";
import { isLoggedIn } from "../../hooks/useAuth";
import { GenericPieChart } from "@/components/Charting/PieChart";

import type {
  TransactionsGetAggregatedTransactionsResponse,
  AggregatedGroup,
} from "../../client";

export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

const availableOptions: GroupByOption[] = [
  GroupByOption.category,
  GroupByOption.year,
  GroupByOption.month,
];

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
  toggleGroup: any;
  setActiveSlice: any;
  expandedGroups: { [key: string]: boolean };
}) {
  return groups.map((group, idx) => {
    const groupKey = pathPrefix
      ? `${pathPrefix}-${group.group_id}`
      : `${group.group_id}`;
    const isExpanded = expandedGroups[`${sourceId}-${groupKey}`] || false;

    return (
      <React.Fragment key={groupKey}>

        <Collapsible.Root open={isExpanded} unmountOnExit>
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
        >

          <TableCell
            pl={pathPrefix ? Number(pathPrefix.split("-").length) * 4 : 0}
          >
            <Collapsible.Trigger asChild onClick={() => toggleGroup(sourceId, groupKey)}>
              {isExpanded ? <ChevronDownIcon /> : <ChevronRightIcon />}
            </Collapsible.Trigger>
          </TableCell>
          <TableCell>{group.group_name}</TableCell>
          <TableCell textAlign="end">{group.total_withdrawals}</TableCell>
          <TableCell textAlign="end">{group.total_deposits}</TableCell>
          <TableCell textAlign="end">{group.total_balance}</TableCell>
        </TableRow>

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
                          <TableColumnHeader textAlign="end">
                            Withdrawals
                          </TableColumnHeader>
                          <TableColumnHeader textAlign="end">
                            Deposits
                          </TableColumnHeader>
                          <TableColumnHeader textAlign="end">
                            Balance
                          </TableColumnHeader>
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
                          <TableColumnHeader textAlign="end">
                            Amount
                          </TableColumnHeader>
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
                            <TableCell textAlign="end">
                              {transaction.amount}
                            </TableCell>
                            <TableCell>{transaction.kind}</TableCell>
                            <TableCell>
                              {transaction.archived ? "Yes" : "No"}
                            </TableCell>
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



export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
});

function Transactions() {
  const [expandedGroups, setExpandedGroups] = useState<{
    [key: string]: boolean;
  }>({});
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([
    GroupByOption.category,
  ]);
  const [activeSlice, setActiveSlice] = useState<{
    [sourceId: number]: number;
  }>({});

  

  const toggleGroupingOption = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      if (prev.includes(option)) {
        if (prev.length === 1) return prev;
        return prev.filter((o) => o !== option);
      } else {
        const newOptions = [...prev, option];
        return availableOptions.filter((o) => newOptions.includes(o));
      }
    });
  };

  const toggleGroup = (sourceId: number, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }));
  };
  const tabs = useTabs({
    defaultValue: "0",
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
  });

  return (
    <Container maxW="full" py={8}>
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} py={12}>
        Transactions
      </Heading>

      <ButtonGroup mb={6} attached variant="outline">
        {availableOptions.map((option) => (
          <Button
            key={option}
            onClick={() => toggleGroupingOption(option)}
            colorScheme={groupingOptions.includes(option) ? "blue" : "gray"}
          >
            {option.charAt(0).toUpperCase() + option.slice(1)}
          </Button>
        ))}
      </ButtonGroup>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data && data.groups && data.groups.length > 0 ? (
        <Tabs.RootProvider variant="enclosed" value={tabs}>
          <Tabs.List>
            {data.groups.map((sourceGroup, index) => (
              <Tabs.Trigger value={index.toString()}>
                {sourceGroup.transaction_source_name}
              </Tabs.Trigger>
            ))}
          </Tabs.List>
            {data.groups.map((sourceGroup, index) => (
              <Tabs.Content value={index.toString()}>
                {groupingOptions.length === 1 && (
                  <Flex gap={4}>
                    <Box flex="1">
                      <GenericPieChart
                        data={sourceGroup.groups.map((group) => ({
                          group: group.group_name,
                          visitors: group.total_deposits,
                        }))}
                        dataKey="visitors"
                        nameKey="group"
                        title="Deposits"
                        config={null}
                        activeIndex={
                          activeSlice[sourceGroup.transaction_source_id] ?? 0
                        }
                      />
                    </Box>
                    <Box flex="1">
                      <GenericPieChart
                        data={sourceGroup.groups.map((group) => ({
                          group: group.group_name,
                          visitors: group.total_withdrawals,
                        }))}
                        dataKey="visitors"
                        nameKey="group"
                        title="Withdrawals"
                        config={null}
                        activeIndex={
                          activeSlice[sourceGroup.transaction_source_id] ?? 0
                        }
                      />
                    </Box>
                  </Flex>
                )}

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
              </Tabs.Content>
            ))}
        </Tabs.RootProvider>
      ) : (
        <Text>No transactions found.</Text>
      )}
    </Container>
  );
}

export default Transactions;
