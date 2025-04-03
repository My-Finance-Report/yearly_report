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

import { FilterData_Input, FilterEntries } from "@/client";
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
  horizontalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable";
import BoxWithText, { type CollapsibleName } from "./BoxWithText";
import WithdrawDepositSelectorSegmented from "./WithdrawDepositSelector";
import { SavedFilterControls } from "./SavedFilterControls";

import { CSS } from "@dnd-kit/utilities";
import { BsFunnel, BsFunnelFill } from "react-icons/bs";
import { FiCheck, FiChevronDown, FiChevronUp } from "react-icons/fi";
import { useIsMobile } from "@/hooks/useIsMobile";
import { useState, useEffect } from "react";
import useAuth from "@/hooks/useAuth";
import { useFilters } from "@/contexts/FilterContext";
import { GroupByOption, GroupingConfig } from "./GroupingConfig";

export function FilterGroup({
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  groupingOptionsChoices: { [key in GroupByOption]: string[] } | undefined;
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
              groupingOptionsChoices={groupingOptionsChoices}
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
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
      groupingOptionsChoices={groupingOptionsChoices}
      setShowDeposits={setShowDeposits}
      showDeposits={showDeposits}
      setCollapsedItems={setCollapsedItems}
      collapsedItems={collapsedItems}
    />
  );
}

function InnerFilterGroup({
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  groupingOptionsChoices: { [Key in GroupByOption]: string[] } | undefined;
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
              groupingOptionsChoices={groupingOptionsChoices}
            />
          {powerUser && (
            <PowerUserButtons
              groupingOptionsChoices={groupingOptionsChoices}
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
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  groupingOptionsChoices: { [Key in GroupByOption]: string[] } | undefined;
}) {
  const includeBudget = !!(
    groupingOptionsChoices &&
    groupingOptionsChoices[GroupByOption.budget]?.length > 1
  );

  const isMobile = useIsMobile();
  const {setCurrentFilter, currentFilter, initializeDefaultFilter} = useFilters();

  if (!currentFilter) {
    initializeDefaultFilter();
  }

  const handleToggleOption = (option: GroupByOption) => {
    setCurrentFilter((prev: FilterData_Input | null) => {
      if (!prev) return prev;
      
      const newLookup = { ...prev.lookup };
      
      if (newLookup[option]) {
        const { [option]: removed, ...rest } = newLookup;
        return { ...prev, lookup: rest };
      } else {
        const maxIndex = Object.values(newLookup).reduce(
          (max, entry) => Math.max(max, (entry as FilterEntries).index), -1
        ) as number;
        
        return {
          ...prev,
          lookup: {
            ...newLookup,
            [option]: {
              all: true,
              visible: true,
              specifics: [],
              index: maxIndex + 1
            }
          }
        };
      }
    });
  };

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;
    if (!over || active.id === over.id || !currentFilter || !currentFilter.lookup) return;

    const activeOption = active.id as GroupByOption;
    const overOption = over.id as GroupByOption;
    
    const activeIndex = currentFilter.lookup[activeOption]?.index;
    const overIndex = currentFilter.lookup[overOption]?.index;
    
    if (activeIndex === undefined || overIndex === undefined) return;
    
    setCurrentFilter((prev: FilterData_Input | null) => {
      if (!prev || !prev.lookup) return prev;
      
      // Create a new lookup object with swapped indices
      const newLookup = { ...prev.lookup };
      
      // Update all indices between activeIndex and overIndex
      Object.entries(newLookup).forEach(([option, entry]) => {
        const optionKey = option as GroupByOption;
        const filterEntry = entry as FilterEntries;
        
        if (activeIndex < overIndex) {
          // Moving down
          if (filterEntry.index > activeIndex && filterEntry.index <= overIndex) {
            newLookup[optionKey] = { 
              ...filterEntry, 
              index: filterEntry.index - 1 
            };
          }
        } else {
          // Moving up
          if (filterEntry.index >= overIndex && filterEntry.index < activeIndex) {
            newLookup[optionKey] = { 
              ...filterEntry, 
              index: filterEntry.index + 1 
            };
          }
        }
      });
      
      // Set the active option to the target index
      newLookup[activeOption] = { 
        ...newLookup[activeOption] as FilterEntries, 
        index: overIndex 
      };
      
      return { ...prev, lookup: newLookup };
    });
  };

  const moveItemUp = (option: GroupByOption) => {
    setCurrentFilter((prev) => {
      if (!prev || !prev.lookup) return prev;
      
      const currentIndex = prev.lookup[option]?.index;
      if (currentIndex === undefined || currentIndex <= 0) return prev;
      
      // Find the option with index = currentIndex - 1
      const optionToSwap = Object.entries(prev.lookup).find(([key, entry]) => (entry as FilterEntries).index === currentIndex - 1)?.[0] as GroupByOption | undefined;
      
      if (!optionToSwap) return prev;
      
      // Create a new lookup object with swapped indices
      const newLookup = { ...prev.lookup };
      newLookup[option] = { ...newLookup[option] as FilterEntries, index: currentIndex - 1 };
      newLookup[optionToSwap] = { ...newLookup[optionToSwap] as FilterEntries, index: currentIndex };
      
      return { ...prev, lookup: newLookup };
    });
  };

  const moveItemDown = (option: GroupByOption) => {
    setCurrentFilter((prev) => {
      if (!prev || !prev.lookup) return prev;
      
      const currentIndex = prev.lookup[option]?.index;
      if (currentIndex === undefined) return prev;
      
      // Find the maximum index in the lookup
      const maxIndex = Math.max(...Object.values(prev.lookup).map(value => (value as FilterEntries).index));
      
      if (currentIndex >= maxIndex) return prev;
      
      // Find the option with index = currentIndex + 1
      const optionToSwap = Object.entries(prev.lookup).find(([_, value]) => (value as FilterEntries).index === currentIndex + 1)?.[0] as GroupByOption | undefined;
      
      if (!optionToSwap) return prev;
      
      // Create a new lookup object with swapped indices
      const newLookup = { ...prev.lookup };
      newLookup[option] = { ...newLookup[option] as FilterEntries, index: currentIndex + 1 };
      newLookup[optionToSwap] = { ...newLookup[optionToSwap] as FilterEntries, index: currentIndex };
      
      return { ...prev, lookup: newLookup };
    });
  };

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor)
  );

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
                items={Object.keys(currentFilter?.lookup || {}).sort((a, b) => 
                  (currentFilter?.lookup?.[a as GroupByOption] as FilterEntries).index - 
                  (currentFilter?.lookup?.[b as GroupByOption] as FilterEntries).index
                )}
                strategy={horizontalListSortingStrategy}
              >
                {Object.entries(currentFilter?.lookup || {})
                  .sort((a, b) => (a[1] as FilterEntries).index - (b[1] as FilterEntries).index)
                  .map(([option, value]) => {
                  const typedOption = option as GroupByOption;
                  const typedValue = value as FilterEntries;
                  return (
                    <Flex
                      paddingLeft={1}
                      marginLeft={typedValue.index * 2}
                      key={option}
                      direction="row"
                      alignItems="center"
                    >
                      <SortableItem
                        key={option}
                        moveItemUp={moveItemUp}
                        moveItemDown={moveItemDown}
                        isFirst={typedValue.index === 0}
                        isLast={typedValue.index === (Object.keys(currentFilter?.lookup || {}).length - 1)}
                        choices={groupingOptionsChoices?.[typedOption]}
                        option={typedOption}
                        noX={Object.keys(currentFilter?.lookup || {}).length === 1}
                        onRemove={handleToggleOption}
                      />
                    </Flex>
                  );
                })}
              </SortableContext>
              <Box key="blah" paddingLeft={Object.keys(currentFilter?.lookup || {}).length * 2}>
                <GroupingConfig
                  showBudgets={includeBudget}
                  groupingOptions={Object.keys(currentFilter?.lookup || {}).map(key => key as GroupByOption)}
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
}

export function FilterButton({
  name,
  options,
}: FilterButtonProps) {
  const { currentFilter, setCurrentFilter } = useFilters();

  if (!currentFilter) {
    return null;
  }

  const handleToggle = (option: string) => {
    if (!currentFilter.lookup) {
      return;
    }
    
    const updatedFilter = { ...currentFilter };
    
    const currentValues = updatedFilter.lookup?.[name]?.specifics || [];
    
    if (currentValues.some(value => value.value === option)) {
      updatedFilter.lookup = {
        ...updatedFilter.lookup,
        [name]: {specifics: currentValues.filter(value => value.value !== option), all: false, visible: true, index: currentFilter.lookup[name].index}
      };

    } else {
      updatedFilter.lookup = {
        ...updatedFilter.lookup,
        [name]: {specifics: [...currentValues, {value: option}], all: false, visible: true, index: currentFilter.lookup[name].index}
      };
    }
    
    setCurrentFilter(updatedFilter);
  };

  const handleSelectAll = () => {
    if (!currentFilter.lookup) {
      return;
    }
    
    const updatedFilter = { ...currentFilter };
    
    updatedFilter.lookup = {
      ...updatedFilter.lookup,
      [name]: {specifics: options.map(option => ({value: option})), all: false, visible: true, index: currentFilter.lookup[name].index}
    };
    
    setCurrentFilter(updatedFilter);
  };

  const handleUnselectAll = () => {
    if (!currentFilter.lookup) {
      return;
    }
    
    const updatedFilter = { ...currentFilter };
    
    updatedFilter.lookup = {
      ...updatedFilter.lookup,
      [name]: {specifics: [], all: false, visible: true, index: currentFilter.lookup[name].index}
    };
    
    setCurrentFilter(updatedFilter);
  };

  const currentValues = currentFilter.lookup?.[name]?.specifics || [];
  const hasApplied = currentValues.length > 0;

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
                const checked = currentValues.map(value => value.value).includes(option);
                return (
                  <Menu.CheckboxItem
                    value={option}
                    key={option}
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
  groupingOptionsChoices,
}: {
  groupingOptionsChoices: Record<GroupByOption, string[]> | undefined;
}) {

  const hasBudgets = groupingOptionsChoices?.[GroupByOption.budget]?.length || 0 > 0;

  const { setCurrentFilter, currentFilter } = useFilters();

  useEffect(() => {
  }, [currentFilter]);

  const setMonthlyBudget = () => {
    const newFilter = {
      is_default: false,
      lookup: {
        [GroupByOption.budget]: {specifics: hasBudgets ? [{value: "Unbudgeted"}] : [], all: false, visible: true, index: 0},
        [GroupByOption.month]: {specifics: [{value: new Date().getMonth().toString()}], all: false, visible: true, index: 1},
        [GroupByOption.year]: {specifics: [{value: new Date().getFullYear().toString()}], all: false, visible: false, index: 2}
      }
    };
    setCurrentFilter(newFilter);
  };

  const setYTD = () => {
    const newFilter = {
      is_default: false,
      lookup: {
        [GroupByOption.month]: {specifics: [{value: new Date().getMonth().toString()}], all: false, visible: true, index: 0},
        [GroupByOption.year]: {specifics: [{value: new Date().getFullYear().toString()}], all: false, visible: false, index: 1}
      }
    };
    
    setCurrentFilter(newFilter);
  };

  const setLastYear = () => {
    const newFilter = {
      is_default: false,
      lookup: {
        [GroupByOption.year]: {specifics: [{value: (new Date().getFullYear() - 1).toString()}], all: false, visible: true, index: 0}
      }
    };
    
    setCurrentFilter(newFilter);
  };

  const setAllTime = () => {
    const newFilter = {
      is_default: false,
      lookup: {
        [GroupByOption.category]: {specifics: [], all: true, visible: true, index: 0}
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
