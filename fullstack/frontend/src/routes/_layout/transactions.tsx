import { useEffect, useState } from "react";

import { Spinner, Text, Box } from "@chakra-ui/react";

import type { CollapsibleName } from "@/components/Common/BoxWithText";
import { FilterGroup, FilterInfo } from "@/components/Common/FilterGroup";
import { GroupByOption } from "@/components/Common/GroupingConfig";
import { Legend } from "@/components/Common/Legend";
import { TransactionsTable } from "@/components/Common/TransactionsTable";
import { VisualizationPanel } from "@/components/Common/VisualizationPanel";
import { useQuery } from "@tanstack/react-query";
import { Link, createFileRoute } from "@tanstack/react-router";
import { AggregatedGroup, DemoService, TransactionsService } from "../../client";
import  { isLoggedIn } from "../../hooks/useAuth";

import { useColorPalette } from "@/hooks/useColor";
import type {
  AggregatedTransactions,
  TransactionsGetAggregatedTransactionsResponse,
} from "../../client";
import { useIsMobile } from "@/hooks/useIsMobile";

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
});

function Transactions() { 
  const getFunction = TransactionsService.getAggregatedTransactions
  return <InnerTransactions getFunction={getFunction} />
}

export function DemoTransactions() { 
  const getFunction = DemoService.getDemoAggregatedTransactions
  return <InnerTransactions getFunction={getFunction} />
}



function InnerTransactions({getFunction}: {
  getFunction: (params: {
    groupBy: GroupByOption[];
    years: string[];
    accounts: string[];
    months: string[];
    categories: string[];
    budgets: string[];
  }) => Promise<TransactionsGetAggregatedTransactionsResponse>;
}) {


  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([GroupByOption.month, GroupByOption.category, GroupByOption.budget]);

  const isMobile = useIsMobile();

  const [accounts, setAccounts] = useState<string[]>([]);
  const [categories, setCategories] = useState<string[]>([]);
  const [months, setMonths] = useState<string[]>([]);
  const [years, setYears] = useState<string[]>([]);
  const [budgets, setBudgets] = useState<string[]>([]);

  const filterInfo: FilterInfo = {
    budgets,
    years,
    accounts,
    months,
    categories,
    setYears,
    setAccounts,
    setMonths,
    setCategories,
    setBudgets,
  };


  const [showDeposits, setShowDeposits] = useState<boolean>(false);
  const [collapsedItems, setCollapsedItems] = useState<CollapsibleName[]>([]);

  const { data, isLoading, error, refetch } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", getFunction.name, groupingOptions],
    queryFn: () =>
      getFunction({
        groupBy: groupingOptions,
        years,
        accounts,
        months,
        categories,
        budgets,
      }),
    enabled: isLoggedIn(),
  });


  useEffect(() => {
    refetch();
  }, [filterInfo]);

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
        gap: "4px",
        marginBottom: isMobile ? 0 : 48,
      }}
    >
      <Box
        maxW="320px"
        style={{
          position: "absolute",
          left: 10,
        }}
      >
        <FilterGroup
          setShowDeposits={setShowDeposits}
          filterInfo={filterInfo}
          groupingOptionsChoices={
            data?.grouping_options_choices as {
              [key in GroupByOption]: string[];
            }
          }
          showDeposits={showDeposits}
          groupingOptions={groupingOptions}
          setGroupingOptions={setGroupingOptions}
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
        />
        <Legend
          collapsedItems={collapsedItems}
          setCollapsedItems={setCollapsedItems}
        />
      </Box>
    <Box marginLeft={isMobile ? 0 : 260} marginTop={isMobile ? '40px' : '0px'}>
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
    <div>
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
        <Link to="/upload-files" href="/upload-files">
          <Text>
            No transactions found.{" "}
            <Text as="span" textDecoration="underline" color="blue.500">
              Click here
            </Text>{" "}
            to upload files.
          </Text>
        </Link>
      )}
    </div>
  );
}

