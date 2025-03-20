import type React from "react";
import {
  DrawerBackdrop,
  DrawerBody,
  DrawerCloseTrigger,
  DrawerContent,
  DrawerFooter,
  DrawerActionTrigger,
  DrawerHeader,
  DrawerRoot,
  DrawerTitle,
  DrawerTrigger,
} from "@/components/ui/drawer";

import { Button, Flex } from "@chakra-ui/react";

import { Box, CloseButton, Tag, Text, Menu, Portal } from "@chakra-ui/react";
import {
  DndContext,
  type DragEndEvent,
  KeyboardSensor,
  PointerSensor,
  closestCenter,
  useSensor,
  useSensors,
} from "@dnd-kit/core";
import {
  SortableContext,
  arrayMove,
  horizontalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable";
import BoxWithText, { type CollapsibleName } from "./BoxWithText";
import { GroupByOption, GroupingConfig } from "./GroupingConfig";
import WithdrawDepositSelectorSegmented from "./WithdrawDepositSelector";

import { CSS } from "@dnd-kit/utilities";
import { BsFunnel, BsFunnelFill } from "react-icons/bs";
import { FiCheck, FiChevronDown, FiChevronUp } from "react-icons/fi";
import { useIsMobile } from "@/hooks/useIsMobile";
import { useState } from "react";
import useAuth from "@/hooks/useAuth";

export interface FilterInfo {
  years: string[];
  accounts: string[];
  months: string[];
  categories: string[];
  budgets: string[];
  setYears: React.Dispatch<React.SetStateAction<string[]>>;
  setAccounts: React.Dispatch<React.SetStateAction<string[]>>;
  setMonths: React.Dispatch<React.SetStateAction<string[]>>;
  setCategories: React.Dispatch<React.SetStateAction<string[]>>;
  setBudgets: React.Dispatch<React.SetStateAction<string[]>>;
}

function getFilterSettings(
  filterInfo: FilterInfo,
  groupingOption: GroupByOption
): {
  values: string[];
  setValues: React.Dispatch<React.SetStateAction<string[]>>;
} {
  switch (groupingOption) {
    case GroupByOption.year:
      return { values: filterInfo.years, setValues: filterInfo.setYears };
    case GroupByOption.account:
      return { values: filterInfo.accounts, setValues: filterInfo.setAccounts };
    case GroupByOption.month:
      return { values: filterInfo.months, setValues: filterInfo.setMonths };
    case GroupByOption.category:
      return {
        values: filterInfo.categories,
        setValues: filterInfo.setCategories,
      };
    case GroupByOption.budget:
      return { values: filterInfo.budgets, setValues: filterInfo.setBudgets };
    default:
      throw "Invalid grouping option";
  }
}

export function FilterGroup({
  filterInfo,
  groupingOptionsChoices,
  groupingOptions,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  filterInfo: FilterInfo;
  groupingOptions: GroupByOption[];
  groupingOptionsChoices: { [key in GroupByOption]: string[] } | undefined;
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>;
  collapsedItems: CollapsibleName[];
}) {
  const isMobile = useIsMobile();

  const [open, setOpen] = useState(false);

  if (isMobile) {
    return (
      <DrawerRoot
        open={open}
        onOpenChange={(e) => setOpen(e.open)}
        placement={"bottom"}
      >
        <DrawerBackdrop />
        <DrawerTrigger asChild>
          <Button variant="outline" size="sm">
            <Text>Adjust Filters</Text>
            <BsFunnel />
          </Button>
        </DrawerTrigger>
        <DrawerContent>
          <DrawerHeader>
            <DrawerTitle></DrawerTitle>
          </DrawerHeader>
          <DrawerBody>
            <InnerFilterGroup
              filterInfo={filterInfo}
              groupingOptionsChoices={groupingOptionsChoices}
              groupingOptions={groupingOptions}
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
              setGroupingOptions={setGroupingOptions}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />
          </DrawerBody>
          <DrawerFooter>
            <DrawerActionTrigger asChild>
              <Button variant="outline">Close</Button>
            </DrawerActionTrigger>
          </DrawerFooter>
          <DrawerCloseTrigger />
        </DrawerContent>
      </DrawerRoot>
    );
  }

  return (
    <InnerFilterGroup
      filterInfo={filterInfo}
      groupingOptionsChoices={groupingOptionsChoices}
      groupingOptions={groupingOptions}
      setShowDeposits={setShowDeposits}
      showDeposits={showDeposits}
      setGroupingOptions={setGroupingOptions}
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
    />
  );
}

function InnerFilterGroup({
  groupingOptions,
  filterInfo,
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  filterInfo: FilterInfo;
  groupingOptions: GroupByOption[];
  groupingOptionsChoices: { [key in GroupByOption]: string[] } | undefined;
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>;
  collapsedItems: CollapsibleName[];
}) {
  const powerUser = useAuth().user?.settings?.power_user_filters ?? false;
  return (
    <div
      style={{
        backgroundColor: "background",
        zIndex: 100,
        minHeight: "150px",
        padding: "1px 0",
        marginBottom: "10px",
      }}
    >
      <div>
        <BoxWithText
          text=""
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
          isCollapsable={false}
          COMPONENT_NAME="Filters"
        >
            <NonPowerUserButtons
              setGroupingOptions={setGroupingOptions}
              filterInfo={filterInfo}
              groupingOptionsChoices={groupingOptionsChoices}
            />
          {powerUser && (
            <PowerUserButtons
              filterInfo={filterInfo}
              setGroupingOptions={setGroupingOptions}
              groupingOptionsChoices={groupingOptionsChoices}
              groupingOptions={groupingOptions}
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
            />
          )
          }
        </BoxWithText>
      </div>
    </div>
  );
}

function PowerUserButtons({
  filterInfo,
  setGroupingOptions,
  groupingOptionsChoices,
  groupingOptions,
  setShowDeposits,
  showDeposits,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  groupingOptions: GroupByOption[];
  filterInfo: FilterInfo;
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
  groupingOptionsChoices: Record<GroupByOption, string[]> | undefined;
}) {
  const includeBudget = !!(
    groupingOptionsChoices &&
    groupingOptionsChoices[GroupByOption.budget]?.length > 1
  );

  const isMobile = useIsMobile();

  const handleToggleOption = (option: GroupByOption) => {
    setGroupingOptions((prev: GroupByOption[]) => {
      return prev.includes(option)
        ? prev.length > 1
          ? prev.filter((o) => o !== option)
          : prev
        : [...prev, option];
    });
  };

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor)
  );

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;
    if (!over || active.id === over.id) return;

    const oldIndex = groupingOptions.indexOf(active.id as GroupByOption);
    const newIndex = groupingOptions.indexOf(over.id as GroupByOption);
    setGroupingOptions(arrayMove(groupingOptions, oldIndex, newIndex));
  };

  const moveItemUp = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      const index = prev.indexOf(option);
      if (index > 0) {
        const newOptions = [...prev];
        [newOptions[index - 1], newOptions[index]] = [
          newOptions[index],
          newOptions[index - 1],
        ];
        return newOptions;
      }
      return prev;
    });
  };

  const moveItemDown = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      const index = prev.indexOf(option);
      if (index < prev.length - 1) {
        const newOptions = [...prev];
        [newOptions[index], newOptions[index + 1]] = [
          newOptions[index + 1],
          newOptions[index],
        ];
        return newOptions;
      }
      return prev;
    });
  };

  return (
    <>
      <Flex
        direction={isMobile ? "column" : "row"}
        gap={4}
        paddingTop={4}
        alignItems={"start"}
        justifyContent={"start"}
      ></Flex>
      <Box mt={4} backgroundColor={"background"} borderRadius="lg">
        <DndContext
          sensors={sensors}
          collisionDetection={closestCenter}
          onDragEnd={handleDragEnd}
        >
          <Flex
            direction="column"
            justifyContent={"start"}
            gap={2}
            alignItems={"start"}
          >
            <Flex direction="column" alignItems="start" gap={2}>
              <WithdrawDepositSelectorSegmented
                setShowDeposits={setShowDeposits}
                showDeposits={showDeposits}
              />
              <SortableContext
                items={groupingOptions}
                strategy={horizontalListSortingStrategy}
              >
                {groupingOptions.map((option, index) => {
                  const { values, setValues } = getFilterSettings(
                    filterInfo,
                    option
                  );
                  return (
                    <Flex
                      paddingLeft={1}
                      marginLeft={index * 2}
                      key={option}
                      direction="row"
                      alignItems="center"
                      justifyContent="start"
                      gap={2}
                    >
                      <SortableItem
                        key={option}
                        filters={values}
                        setFilters={setValues}
                        moveItemUp={moveItemUp}
                        moveItemDown={moveItemDown}
                        isFirst={index === 0}
                        isLast={index === groupingOptions.length - 1}
                        choices={groupingOptionsChoices?.[option]}
                        option={option}
                        noX={groupingOptions.length === 1}
                        onRemove={handleToggleOption}
                      />
                    </Flex>
                  );
                })}
              </SortableContext>
              <Box key="blah" paddingLeft={groupingOptions.length * 2}>
                <GroupingConfig
                  showBudgets={includeBudget}
                  groupingOptions={groupingOptions}
                  setGroupingOptions={setGroupingOptions}
                />
              </Box>
            </Flex>
          </Flex>
        </DndContext>
      </Box>
    </>
  );
}

const SortableItem = ({
  option,
  filters,
  setFilters,
  onRemove,
  noX,
  choices,
  moveItemUp,
  moveItemDown,
  isFirst,
  isLast,
}: {
  option: GroupByOption;
  choices: string[] | undefined;
  filters: string[];
  setFilters: React.Dispatch<React.SetStateAction<string[]>>;
  noX: boolean;
  onRemove: (option: GroupByOption) => void;
  moveItemUp: (option: GroupByOption) => void;
  moveItemDown: (option: GroupByOption) => void;
  isFirst: boolean;
  isLast: boolean;
}) => {
  const { attributes, listeners, setNodeRef, transform, transition } =
    useSortable({
      id: option,
    });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  };

  return (
    <Tag.Root
      ref={setNodeRef}
      py={noX ? 2 : 0}
      minW={"260px"}
      justifyContent="space-between"
      style={style}
      {...attributes}
      {...listeners}
    >
      <Text ml={1} cursor="grab">
        {option.charAt(0).toUpperCase() + option.slice(1)}
      </Text>
      <Flex p={1} gap={0}>
        <FilterButton
          filters={filters}
          setFilters={setFilters}
          options={choices ?? []}
          name={option}
        />
        <Button
          size="xs"
          variant="subtle"
          disabled={isFirst}
          onClick={() => moveItemUp(option)}
        >
          <FiChevronUp />
        </Button>
        <Button
          size="xs"
          variant="subtle"
          disabled={isLast}
          onClick={() => moveItemDown(option)}
        >
          <FiChevronDown />
        </Button>
        {!noX && (
          <CloseButton
            onClick={() => onRemove(option)}
            size="xs"
            variant="subtle"
          />
        )}
      </Flex>
    </Tag.Root>
  );
};
interface FilterButtonProps {
  options: string[];
  name: string;
  filters: string[];
  setFilters: React.Dispatch<React.SetStateAction<string[]>>;
}

export function FilterButton({
  options,
  filters,
  setFilters,
}: FilterButtonProps) {
  const handleToggle = (option: string) => {
    if (filters.includes(option)) {
      setFilters(filters.filter((f) => f !== option));
    } else {
      setFilters([...filters, option]);
    }
  };

  const handleSelectAll = () => {
    setFilters(options);
  };

  const handleUnselectAll = () => {
    setFilters([]);
  };

  const hasApplied = filters.length > 0;

  return (
    <Menu.Root
      closeOnSelect={false}
      onInteractOutside={() => {}}
      onPointerDownOutside={() => {}}
      onExitComplete={() => {}}
      onEscapeKeyDown={() => {}}
    >
      <Menu.Trigger asChild>
        <Button variant="subtle" size="xs">
          {hasApplied ? <BsFunnelFill /> : <BsFunnel />}
        </Button>
      </Menu.Trigger>
      <Portal>
        <Menu.Positioner>
          <Menu.Content zIndex={10000}>
            <Flex direction="column" gap={2}>
              <Button size="xs" variant="subtle" onClick={handleSelectAll}>
                Select All
              </Button>
              <Button size="xs" variant="subtle" onClick={handleUnselectAll}>
                Unselect All
              </Button>
            </Flex>
            <Menu.ItemGroup>
              {options.map((option) => {
                const checked = filters.includes(option);
                return (
                  <Menu.CheckboxItem
                    value={option}
                    key={option}
                    // The key part is controlling 'checked' and toggling via parent
                    checked={checked}
                    onCheckedChange={() => handleToggle(option)}
                  >
                    {option.charAt(0).toUpperCase() + option.slice(1)}
                    {checked && <FiCheck />}
                  </Menu.CheckboxItem>
                );
              })}
            </Menu.ItemGroup>
          </Menu.Content>
        </Menu.Positioner>
      </Portal>
    </Menu.Root>
  );
}

function NonPowerUserButtons({
  filterInfo,
  setGroupingOptions,
  groupingOptionsChoices,
}: {
  filterInfo: FilterInfo;
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
  groupingOptionsChoices: Record<GroupByOption, string[]> | undefined;
}) {
  const excludingUnbudgeted =
    groupingOptionsChoices?.[GroupByOption.budget]?.filter(
      (budget) => budget !== "Unbudgeted"
    ) ?? [];

  const { setValues: setYears } = getFilterSettings(
    filterInfo,
    GroupByOption.year
  );
  const { setValues: setBudgets } = getFilterSettings(
    filterInfo,
    GroupByOption.budget
  );

  const setMonthlyBudget = () => {
    setGroupingOptions([
      GroupByOption.budget,
      GroupByOption.month,
      GroupByOption.year,
    ]);
    setYears([new Date().getFullYear().toString()]);
    setBudgets(excludingUnbudgeted);
  };

  const setYTD = () => {
    setGroupingOptions([
      GroupByOption.month,
      GroupByOption.category,
      GroupByOption.year,
    ]);
    setYears([new Date().getFullYear().toString()]);
  };

  const setLastYear = () => {
    setGroupingOptions([
      GroupByOption.month,
      GroupByOption.category,
      GroupByOption.year,
    ]);
    setYears([(new Date().getFullYear() - 1).toString()]);
  };

  return (
    <Flex gap={2} direction={"column"}>
      <Button size="xs" variant="subtle" onClick={setMonthlyBudget}>
        Monthly Budget
      </Button>
      <Button size="xs" variant="subtle" onClick={setYTD}>
        Year To Date
      </Button>
      <Button size="xs" variant="subtle" onClick={setLastYear}>
        Last Year
      </Button>
    </Flex>
  );
}
