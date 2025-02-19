import { Box, Flex } from "@chakra-ui/react";
import { GenericPieChart } from "../Charting/PieChart";
import type { TransactionSourceGroup } from "../../client";

export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

interface VisualizationProps {
  activeSlice: { [sourceId: number]: number };
  sourceGroup: TransactionSourceGroup;
}

export function VisualizationPanel({
  activeSlice,
  sourceGroup,
}: VisualizationProps) {
  return (
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
    </Flex>
  );
}
