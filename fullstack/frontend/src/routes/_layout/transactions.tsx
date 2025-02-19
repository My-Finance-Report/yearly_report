import { useState } from "react";
import { useTabs } from "@chakra-ui/react";

import {
  Container,
  Spinner,
  Text,
  Tabs,
} from "@chakra-ui/react";

import { createFileRoute } from "@tanstack/react-router";
import { useQuery } from "@tanstack/react-query";
import { TransactionsService } from "../../client";
import { isLoggedIn } from "../../hooks/useAuth";
import { TransactionsTable } from "@/components/Common/TransactionsTable";
import { VisualizationPanel } from "@/components/Common/VisualizationPanel";
import { GroupByOption, GroupingConfig } from "@/components/Common/GroupingConfig";

import type {
  TransactionsGetAggregatedTransactionsResponse,
} from "../../client";

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
});

function Transactions() {
  const [expandedGroups, setExpandedGroups] = useState<{ [key: string]: boolean }>({});
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([GroupByOption.category]);
  const [activeSlice, setActiveSlice] = useState<{ [sourceId: number]: number }>({});
  const [activeGroup, setActiveGroup] = useState<number>(0);



  const toggleGroup = (sourceId: number, groupKey: string) => {
    setExpandedGroups((prev) => ({
      ...prev,
      [`${sourceId}-${groupKey}`]: !prev[`${sourceId}-${groupKey}`],
    }));
  };

  const tabs = useTabs({
    defaultValue: "0",
    onFocusChange: (value) => setActiveGroup(Number(value.focusedValue)),
  });

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
    <Container maxW="large" py={8}>

      <VisualizationPanel activeSlice={activeSlice} sourceGroup={data?.groups[activeGroup]} isLoading={isLoading} />

      <GroupingConfig
        groupingOptions={groupingOptions}
        setGroupingOptions={setGroupingOptions}
      />

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data && data.groups && data.groups.length > 0 ? (
        <Tabs.RootProvider variant="enclosed" value={tabs}>

          <Tabs.List>
            {data.groups.map((sourceGroup, index) => (
              <Tabs.Trigger key={index} value={index.toString()}>
                {sourceGroup.transaction_source_name}
              </Tabs.Trigger>
            ))}
          </Tabs.List>
          {data.groups.map((sourceGroup, index) => (
            <Tabs.Content key={index} value={index.toString()}>
              <TransactionsTable
                toggleGroup={toggleGroup}
                sourceGroup={sourceGroup}
                setActiveSlice={setActiveSlice}
                expandedGroups={expandedGroups}
              />
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
