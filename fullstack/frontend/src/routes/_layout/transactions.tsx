"use client";

import { useState } from "react";
import {
  useTabs
} from "@chakra-ui/react";

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
import { TransactionsTable } from "@/components/Common/TransactionsTable";

import type {
  TransactionsGetAggregatedTransactionsResponse,
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
                <TransactionsTable toggleGroup={toggleGroup}  sourceGroup={sourceGroup} setActiveSlice={setActiveSlice} expandedGroups={expandedGroups} />
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
