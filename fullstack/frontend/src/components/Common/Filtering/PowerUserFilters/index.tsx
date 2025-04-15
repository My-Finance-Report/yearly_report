import { Route } from "@/routes/_layout/_logged_in/transactions"
import type {  FilterEntries } from "@/client";
import { Button, Flex } from "@chakra-ui/react";
import { handleDragEnd, moveItemDown,breakoutToCustomFilter, moveItemUp } from "./DragHelpers";

import WithdrawDepositSelectorSegmented from "@/components/Common/WithdrawDepositSelector";
import { Box, CloseButton, Menu, Portal, Tag, Text } from "@chakra-ui/react";
import {
  DndContext,
  KeyboardSensor,
  PointerSensor,
  closestCenter,
  useSensor,
  useSensors,
} from "@dnd-kit/core";
import {
  SortableContext,
  useSortable,
  verticalListSortingStrategy,
} from "@dnd-kit/sortable";

import {
  GroupByOption,
  GroupingConfig,
} from "@/components/Common/GroupingConfig";
import { useFilters } from "@/contexts/FilterContext";
import { useIsMobile } from "@/hooks/useIsMobile";
import { CSS } from "@dnd-kit/utilities";
import { BsFunnel, BsFunnelFill } from "react-icons/bs";

import {
  FiCheck,
  FiChevronDown,
  FiChevronUp,
  FiEye,
  FiEyeOff,
} from "react-icons/fi";
import { useNavigate } from "@tanstack/react-router";


export function PowerUserButtons({
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
  const {  currentFilter } = useFilters();

  const handleToggleOption = (option: GroupByOption) => {

      const newLookup = { ...currentFilter.filter_data.lookup };

      if (newLookup[option]) {
        const rest = { ...newLookup };
        delete rest[option];
        return breakoutToCustomFilter({ ...currentFilter.filter_data, lookup: rest });
      }

      const maxIndex = Object.values(newLookup).reduce(
        (max, entry) => Math.max(max, (entry as FilterEntries).index),
        -1
      ) as number;


    return breakoutToCustomFilter({
      lookup: {
        ...newLookup,
        [option]: {
          visible: true,
          specifics: [],
            index: maxIndex + 1,
          },
        },
      },
    );
  };

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor)
  );

  const { showAdvanced } = Route.useSearch()
  const navigation = useNavigate();
  const handleShowAdvanced = () => {
    navigation({
      search: (prev: Record<string, unknown>) => ({
        ...prev,
        showAdvanced: true,
      }),
      replace: true,
    });
  };
  if (!showAdvanced) {
    return (
      <Button variant="plain" size="sm" onClick={handleShowAdvanced}>
        <Text textDecoration={"underline"}>
          Show Advanced Filtering Options
        </Text>{" "}
        <FiEye />
      </Button>
    );
  }

  return (
    <>
      <Flex
        direction={isMobile ? "column" : "row"}
        gap={4}
        paddingTop={4}
        alignItems={"start"}
        justifyContent={"start"}
      />
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
                items={Object.keys(
                  currentFilter?.filter_data?.lookup || {}
                ).sort(
                  (a, b) =>
                    (
                      currentFilter?.filter_data?.lookup?.[
                        a as GroupByOption
                      ] as FilterEntries
                    ).index -
                    (
                      currentFilter?.filter_data?.lookup?.[
                        b as GroupByOption
                      ] as FilterEntries
                    ).index
                )}
                strategy={verticalListSortingStrategy}
              >
                {Object.entries(currentFilter?.filter_data?.lookup || {})
                  .sort(
                    (a, b) =>
                      (a[1] as FilterEntries).index -
                      (b[1] as FilterEntries).index
                  )
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
                          isLast={
                            typedValue.index ===
                            Object.keys(
                              currentFilter?.filter_data?.lookup || {}
                            ).length -
                              1
                          }
                          choices={groupingOptionsChoices?.[typedOption]}
                          option={typedOption}
                          noX={
                            Object.keys(
                              currentFilter?.filter_data?.lookup || {}
                            ).length === 1
                          }
                          onRemove={handleToggleOption}
                        />
                      </Flex>
                    );
                  })}
              </SortableContext>
              <Box
                key="blah"
                paddingLeft={
                  Object.keys(currentFilter?.filter_data?.lookup || {}).length *
                  2
                }
              >
                <GroupingConfig
                  showBudgets={includeBudget}
                  groupingOptions={Object.keys(
                    currentFilter?.filter_data?.lookup || {}
                  ).map((key) => key as GroupByOption)}
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

  const { currentFilter, setCurrentFilter } = useFilters();

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  };

  const isVisible =
    currentFilter?.filter_data?.lookup?.[option]?.visible ?? true;

  const toggleVisibility = () => {
    if (!currentFilter?.filter_data?.lookup) return;

    setCurrentFilter((prev) => {
      if (!prev?.filter_data?.lookup) return prev;

      return {
        ...prev,
        filter_data: {
          ...prev.filter_data,
          lookup: {
            ...prev.filter_data.lookup,
            [option]: {
              ...prev.filter_data.lookup[option],
              visible: !isVisible,
            },
          },
        },
      };
    });
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
        <FilterButton options={choices ?? []} name={option} />
        <Button
          size="xs"
          variant="subtle"
          onClick={toggleVisibility}
          title={isVisible ? "Hide this grouping" : "Show this grouping"}
        >
          {isVisible ? <FiEye /> : <FiEyeOff />}
        </Button>
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

export function FilterButton({ name, options }: FilterButtonProps) {
  const { currentFilter, setCurrentFilter } = useFilters();

  if (!currentFilter) {
    return null;
  }

  const handleToggle = (option: string) => {
    if (!currentFilter.filter_data.lookup) {
      return;
    }

    const updatedFilter = { ...currentFilter };

    const currentValues =
      updatedFilter.filter_data.lookup?.[name]?.specifics || [];

    if (currentValues.some((value) => value.value === option)) {
      updatedFilter.filter_data.lookup = {
        ...updatedFilter.filter_data.lookup,
        [name]: {
          specifics: currentValues.filter((value) => value.value !== option),
          visible: true,
          index: currentFilter.filter_data.lookup[name].index,
        },
      };
    } else {
      updatedFilter.filter_data.lookup = {
        ...updatedFilter.filter_data.lookup,
        [name]: {
          specifics: [...currentValues, { value: option }],
          visible: true,
          index: currentFilter.filter_data.lookup[name].index,
        },
      };
    }

    setCurrentFilter(updatedFilter);
  };

  const handleSelectAll = () => {
    if (!currentFilter.filter_data.lookup) {
      return;
    }

    const updatedFilter = { ...currentFilter };

    updatedFilter.filter_data.lookup = {
      ...updatedFilter.filter_data.lookup,
      [name]: {
        specifics: options.map((option) => ({ value: option })),
        visible: true,
        index: currentFilter.filter_data.lookup[name].index,
      },
    };

    setCurrentFilter(updatedFilter);
  };

  const handleUnselectAll = () => {
    if (!currentFilter.filter_data.lookup) {
      return;
    }

    const updatedFilter = { ...currentFilter };

    updatedFilter.filter_data.lookup = {
      ...updatedFilter.filter_data.lookup,
      [name]: {
        specifics: [],
        visible: true,
        index: currentFilter.filter_data.lookup[name].index,
      },
    };

    setCurrentFilter(updatedFilter);
  };

  const currentValues =
    currentFilter.filter_data.lookup?.[name]?.specifics || [];
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
                const checked = currentValues
                  .map((value) => value.value)
                  .includes(option);
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
