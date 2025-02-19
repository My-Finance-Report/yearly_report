import { Box, Flex, Spinner, Text } from "@chakra-ui/react";
import { GenericPieChart } from "../Charting/PieChart";
import { GenericBarChart } from "../Charting/BarChart";
import type { TransactionSourceGroup, GroupByOption, AggregatedGroup } from "../../client";



interface VisualizationProps {
  activeSlice: { [sourceId: number]: number };
  sourceGroup: TransactionSourceGroup | undefined;
  isLoading: boolean;
}

interface ValidatedVisualizationProps {
  activeSlice: { [sourceId: number]: number };
  sourceGroup: TransactionSourceGroup
  isLoading: boolean;
}

export function VisualizationPanel({
  activeSlice,
  sourceGroup,
  isLoading,
}: VisualizationProps) {
  return (
    <Flex gap={4} minH="300px" align="center" justify="center">
      {isLoading || !sourceGroup ? (
        <Spinner size="lg" />
      ) : (
        <>
          <PieBox sourceGroup={sourceGroup} activeSlice={activeSlice} isLoading={isLoading} />
          <BarChart sourceGroup={sourceGroup} activeSlice={activeSlice} isLoading={isLoading} />
        </>
      )}
    </Flex>
  );
}


function BarChart({ sourceGroup }: ValidatedVisualizationProps) {
  const TIME_OPTIONS: GroupByOption[] = ['month', 'year']
  const hasTimeGrouping = (group: AggregatedGroup): boolean => {
    if (group.groupby_kind && TIME_OPTIONS.includes(group.groupby_kind)) {
      return true;
    }

    return group.subgroups?.some(hasTimeGrouping) || false;
  };

  const hasValidTimeGrouping = sourceGroup.groups.some(hasTimeGrouping);


  const chartData = hasValidTimeGrouping
    ? sourceGroup.groups.map((group) => ({
      date: group.group_id.toString(),
      deposits: group.total_deposits,
      withdrawals: group.total_withdrawals,
    }))
    : [];

  return hasValidTimeGrouping ? (
    <GenericBarChart
      data={chartData}
      dataKey="deposits"
      nameKey="date"
      title={`Deposits & Withdrawals for ${sourceGroup.transaction_source_name}`}
      config={{
        deposits: { label: "Deposits", color: "blue.400" },
        withdrawals: { label: "Withdrawals", color: "red.400" },
      }}
    />
  ) : (
    <Box textAlign="center" p={4}>
      <Text fontSize="lg" color="gray.500">
        This grouping configuration does not support a bar chart. Please include a time-based grouping (e.g., month or year).
      </Text>
    </Box>
  );
}

function PieBox({ sourceGroup, activeSlice }: ValidatedVisualizationProps) {

  return (
    <>
      <Box flex="1">
        <GenericPieChart
          data={sourceGroup.groups.map((group) => ({
            group: group.group_name,
            amount: group.total_deposits,
          }))}
          dataKey="visitors"
          nameKey="group"
          title="Deposits"
          config={null}
          activeIndex={activeSlice[sourceGroup.transaction_source_id] ?? 0}
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
          activeIndex={activeSlice[sourceGroup.transaction_source_id] ?? 0}
        />
      </Box>
    </>
  )
}