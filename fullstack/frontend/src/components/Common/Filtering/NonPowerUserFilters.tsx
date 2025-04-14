import type React from "react";
import { FilterData_Input} from "@/client";
import { Button, Flex } from "@chakra-ui/react";

import { Box} from "@chakra-ui/react";
import { SavedFilterControls } from "@/components/Common/SavedFilterControls";

import {  useEffect } from "react";
import { useFilters } from "@/contexts/FilterContext";
import { GroupByOption } from "@/components/Common/GroupingConfig";



export function NonPowerUserButtons({
  groupingOptionsChoices,
}: {
  groupingOptionsChoices: Record<GroupByOption, string[]> | undefined;
}) {

  const hasBudgets = groupingOptionsChoices?.[GroupByOption.budget]?.length || 0 > 0;

  const { setCurrentFilter, currentFilter } = useFilters();

  useEffect(() => {
  }, [currentFilter]);

  const setMonthlyBudget = () => {
    const newFilter:FilterData_Input = {
      is_default: false,
      lookup: {
        [GroupByOption.budget]: {specifics: null, visible: true, index: 0},
        [GroupByOption.month]: {specifics: [{value: new Date().getMonth().toString()}], visible: true, index: 1},
        [GroupByOption.year]: {specifics: [{value: new Date().getFullYear().toString()}], visible: false, index: 2}
      }
    };
    setCurrentFilter(newFilter);
  };

  const setYTD = () => {
    const newFilter:FilterData_Input = {
      is_default: false,
      lookup: {
        [GroupByOption.month]: {specifics: null, visible: true, index: 0},
        [GroupByOption.year]: {specifics: [{value: new Date().getFullYear().toString()}], visible: false, index: 1},
        [GroupByOption.category]: {specifics: null, visible: true, index: 0},
      }
    };
    
    setCurrentFilter(newFilter);
  };

  const setLastYear = () => {
    const newFilter:FilterData_Input = {
      is_default: false,
      lookup: {
        [GroupByOption.year]: {specifics: [{value: (new Date().getFullYear() - 1).toString()}], visible: false, index: 0},
        [GroupByOption.category]: {specifics: null, visible: true, index: 0},
        [GroupByOption.account]: {specifics: null, visible: true, index: 0}
      }
    };
    
    setCurrentFilter(newFilter);
  };

  const setAllTime = () => {
    const newFilter:FilterData_Input = {
      is_default: false,
      lookup: {
        [GroupByOption.category]: {specifics: null, visible: true, index: 0}
      }
    };
    
    setCurrentFilter(newFilter);
  };

  return (
    <Box>
      <Flex justifyContent="space-between" alignItems="center" mb={4}>
        <SavedFilterControls />
      </Flex>
      <Flex direction="column" gap={2}>
        {hasBudgets && (
        <Button size="xs" variant="subtle" onClick={setMonthlyBudget}>
          Monthly Budget
        </Button>
        )}
        <Button size="xs" variant="subtle" onClick={setYTD}>
          Year To Date
        </Button>
        <Button size="xs" variant="subtle" onClick={setLastYear}>
          Last Year
        </Button>
        <Button size="xs" variant="subtle" onClick={setAllTime}>
          All Time
        </Button>
      </Flex>
    </Box>
  );
}
