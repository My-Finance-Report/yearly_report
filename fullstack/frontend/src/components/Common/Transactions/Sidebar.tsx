
import { Box } from "@chakra-ui/react";
import type { CollapsibleName } from "@/components/Common/BoxWithText";
import { FilterGroup } from "@/components/Common/FilterGroup";
import { GroupByOption } from "@/components/Common/GroupingConfig";
import { CollapsedItems } from "@/components/Common/Legend";
import type {
  AggregatedTransactions,
} from "@/client";



export function MainLayoutSidebar({
  setShowDeposits,
  data,
  showDeposits,
  collapsedItems,
  setCollapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  data: AggregatedTransactions | undefined;
  showDeposits: boolean;
  collapsedItems: CollapsibleName[];
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>;
}) {
  return (
    <Box as={"aside"} position={"absolute"} top={"80px"} left={"10px"}>
      <FilterGroup
        setShowDeposits={setShowDeposits}
        groupingOptionsChoices={
          data?.grouping_options_choices as { [key in GroupByOption]: string[] }
        }
        showDeposits={showDeposits}
      />
      <CollapsedItems
        collapsedItems={collapsedItems}
        setCollapsedItems={setCollapsedItems}
      />
    </Box>
  );
}