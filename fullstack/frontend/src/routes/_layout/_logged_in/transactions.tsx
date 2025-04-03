import { useEffect, useState } from "react";

import { Spinner, Text, Box, VStack, Heading, HStack, Button } from "@chakra-ui/react";
import { FaLink, FaUpload } from "react-icons/fa";
import { Link, createFileRoute } from "@tanstack/react-router";
import { z } from "zod";

import type { CollapsibleName } from "@/components/Common/BoxWithText";
import { FilterGroup } from "@/components/Common/FilterGroup";
import { GroupByOption } from "@/components/Common/GroupingConfig";
import { Legend } from "@/components/Common/Legend";
import { TransactionsTable } from "@/components/Common/TransactionsTable";
import { VisualizationPanel } from "@/components/Common/VisualizationPanel";
import { useQuery } from "@tanstack/react-query";
import { AggregatedGroup, DemoService, TransactionsService } from "@/client";
import { isLoggedIn } from "@/hooks/useAuth";

import { useColorPalette } from "@/hooks/useColor";
import type {
  AggregatedTransactions,
  TransactionsGetAggregatedTransactionsData,
  TransactionsGetAggregatedTransactionsResponse,
} from "@/client";
import { useIsMobile } from "@/hooks/useIsMobile";
import { FilterProvider, useFilters } from "@/contexts/FilterContext";

const transactionsSearchSchema = z.object({
  filter: z.string().optional(),
});

export const Route = createFileRoute("/_layout/_logged_in/transactions")({
  component: Transactions,
  validateSearch: (search) => transactionsSearchSchema.parse(search),
});

function Transactions() { 
  const getFunction = TransactionsService.getAggregatedTransactions
  return (
    <FilterProvider>
      <InnerTransactions getFunction={getFunction} />
    </FilterProvider>
  )
}

export function DemoTransactions() { 
  const getFunction = DemoService.getDemoAggregatedTransactions
  return (
    <FilterProvider>
      <InnerTransactions getFunction={getFunction} />
    </FilterProvider>
  )
}

function InnerTransactions({getFunction}: {
  getFunction: (
  data:TransactionsGetAggregatedTransactionsData 
  ) => Promise<TransactionsGetAggregatedTransactionsResponse>;
}) {

  const isMobile = useIsMobile();

  const [showDeposits, setShowDeposits] = useState<boolean>(false);
  const [collapsedItems, setCollapsedItems] = useState<CollapsibleName[]>([]);

  const {currentFilter, initializeDefaultFilter} = useFilters();

  useEffect(() => {
    if (!currentFilter) {
      initializeDefaultFilter();
    }
  }, [initializeDefaultFilter, currentFilter]);

  const { data, isLoading, error, refetch } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", getFunction.name, currentFilter],
    queryFn: () =>
    {
      return getFunction({requestBody : currentFilter})
    },
    enabled: isLoggedIn() && !!currentFilter,
  });


  useEffect(() => {
    refetch();
  }, [currentFilter]);

  const { getColorForName } = useColorPalette();

  data?.groups.map((group) => {
    getColorForName(group.group_name);
    group.subgroups?.map((subgroup) => {
      getColorForName(subgroup.group_name);
    });
  });

  const [activeGrouping, setActiveGrouping] = useState<
    AggregatedGroup[] | null
  >(null);

  useEffect(() => {
    if (data?.groups.length) {
      setActiveGrouping(data.groups);
    }
  }, [data?.groups]);

  const hasData = data?.groups && activeGrouping && data.grouping_options_choices


  const namesForLegends = data?.groups.flatMap((group) =>
    group?.subgroups?.map((subgroup) => subgroup.group_name)
  );

  if (error) {
    return <Text color="red.500">Error loading transactions.</Text>;
  }

  return (
    <div
      style={{
        display: "flex",
        flexDirection: isMobile ? "column" : "row",
        justifyContent:  "center",
        gap: "4px",
        marginBottom: isMobile ? 0 : 48,
        padding: "10px",
      }}
    >
      {hasData && (
      <Box
      >
        <FilterGroup
          setShowDeposits={setShowDeposits}
          groupingOptionsChoices={
            data?.grouping_options_choices as { [key in GroupByOption]: string[] } 
          }
          showDeposits={showDeposits}
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
        />
        <Legend
          collapsedItems={collapsedItems}
          setCollapsedItems={setCollapsedItems}
        />
      </Box>
      )}
      <Box marginTop={isMobile ? '40px' : '0px'}>
        <BlahComponent
          isLoading={isLoading}
          data={data}
          activeGrouping={activeGrouping}
          showDeposits={showDeposits}
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
          namesForLegends={namesForLegends}
          isMobile={isMobile}
        />
      </Box>
    </div>
  );
}

function BlahComponent({
  isLoading,
  data,
  activeGrouping,
  showDeposits,
  setCollapsedItems,
  collapsedItems,
  namesForLegends,
  isMobile,
}: {
  isLoading: boolean;
  data: AggregatedTransactions | undefined;
  activeGrouping: AggregatedGroup[] | null;
  showDeposits: boolean;
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>;
  collapsedItems: CollapsibleName[];
  namesForLegends: (string | undefined)[] | undefined;
  isMobile: boolean;
}) {
  if (isLoading) {
    return (
      <Box>
        <Spinner />
      </Box>
    );
  }

  return (
    <Box >
      {data?.groups && activeGrouping ? (
        <div
          style={{
            flexDirection: "column",
            justifyContent: "center",
            alignItems: "start",
          }}
        >
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
        </div>
      ) : (
        
            <NullState />
      )}
    </Box>
  );
}


function NullState(){
  return (
    <Box 
      p={8} 
      textAlign="center" 
      borderWidth="1px" 
      borderRadius="lg" 
      boxShadow="sm"
    >
      <VStack gap={6}>
        <Heading size="md">Welcome Aboard!</Heading>
        <Text >
          Get started by connecting accounts or uploading statements 
        </Text>
        <HStack gap={4} pt={4}>
          <Link to="/plaid">
            <Button 
              variant="solid"
            >
              <Box mr={2} display="inline-block"><FaLink /></Box>
              Link Accounts
            </Button>
          </Link>
          <Link to="/upload-files">
            <Button 
              variant="outline"
            >
              <Box mr={2} display="inline-block"><FaUpload /></Box>
              Upload Files
            </Button>
          </Link>
        </HStack>
      </VStack>
    </Box>
  )
}