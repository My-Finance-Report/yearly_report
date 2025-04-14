import { Spinner, Box, Text, Flex } from "@chakra-ui/react";
import type { CollapsibleName } from "@/components/Common/BoxWithText";
import { TransactionsTable } from "@/components/Common/TransactionsTable";
import { VisualizationPanel } from "@/components/Common/VisualizationPanel";
import { AggregatedGroup, DemoService, TransactionsService } from "@/client";
import { MainLayoutSidebar } from "@/components/Common/Transactions/Sidebar";

import { useIsMobile } from "@/hooks/useIsMobile";
import { useEffect, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import type {
  TransactionsGetAggregatedTransactionsResponse,
} from "@/client";
import { useFilters } from "@/contexts/FilterContext";

export function TransactionsView({isDemo}: {isDemo: boolean}) {

  const getFunction = isDemo ? DemoService.getDemoAggregatedTransactions : TransactionsService.getAggregatedTransactions;


  const [showDeposits, setShowDeposits] = useState<boolean>(false);
  const [collapsedItems, setCollapsedItems] = useState<CollapsibleName[]>([]);

  const { currentFilter, initializeDefaultFilter } = useFilters();

  useEffect(() => {
    if (!currentFilter) {
      initializeDefaultFilter();
    }
  }, [initializeDefaultFilter, currentFilter]);


  const { data, isLoading, refetch } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", getFunction.name, currentFilter],
    queryFn: () => {
      return getFunction({ requestBody: currentFilter });
    },
  });

  useEffect(() => {
    refetch();
  }, [currentFilter]);


  const [activeGrouping, setActiveGrouping] = useState<
    AggregatedGroup[] | null
  >(null);

  useEffect(() => {
    if (data?.groups.length) {
      setActiveGrouping(data.groups);
    }
  }, [data?.groups]);

  const namesForLegends = data?.groups.flatMap((group) =>
    group?.subgroups?.map((subgroup) => subgroup.group_name)
  );

  const isMobile = useIsMobile();

  const hasData = data?.groups && data.groups.length > 0;

  if (isLoading) {
    return (
      <Box>
        <Spinner />
      </Box>
    );
  }

  return (
    <Box
    mx={isMobile ? 1 : 3}
    >
      {!isDemo && (
        <MainLayoutSidebar
          setShowDeposits={setShowDeposits}
          data={data}
          showDeposits={showDeposits}
          collapsedItems={collapsedItems}
        setCollapsedItems={setCollapsedItems}
      />)}
      <Box marginTop={isMobile ? "40px" : "0px"} marginLeft={isMobile || isDemo ? "0px" : "320px"}>
          <Box gap={3} display="flex" flexDirection="column" justifyContent="start" alignItems="start">
            {hasData ? (
              <>
              <VisualizationPanel
                sourceGroups={activeGrouping}
                isLoading={isLoading}
                showDeposits={showDeposits}
                  setCollapsedItems={setCollapsedItems}
                collapsedItems={collapsedItems}
              />
              <TransactionsTable
                data={data}
                toShowNames={namesForLegends}
                showWithdrawals={!showDeposits}
                isMobile={isMobile}
              />
              </>
            ) : (
              <Flex justifyContent="center" alignItems="center">
                <Text>The filters you set don't yield any transactions :(</Text>
              </Flex>
            )}
          </Box>
      </Box>
    </Box>
  );
}


