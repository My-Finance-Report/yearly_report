import type React from "react"

import type { FilterData_Input, FilterEntries } from "@/client"
import { Button, Flex } from "@chakra-ui/react"
import { handleDragEnd, moveItemDown, moveItemUp } from "./DragHelpers"

import WithdrawDepositSelectorSegmented from "@/components/Common/WithdrawDepositSelector"
import { Box, CloseButton, Menu, Portal, Tag, Text } from "@chakra-ui/react"
import {
  DndContext,
  KeyboardSensor,
  PointerSensor,
  closestCenter,
  useSensor,
  useSensors,
} from "@dnd-kit/core"
import {
  SortableContext,
  horizontalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable"

import {
  GroupByOption,
  GroupingConfig,
} from "@/components/Common/GroupingConfig"
import { useFilters } from "@/contexts/FilterContext"
import { useIsMobile } from "@/hooks/useIsMobile"
import { CSS } from "@dnd-kit/utilities"
import { BsFunnel, BsFunnelFill } from "react-icons/bs"
import {
  FiCheck,
  FiChevronDown,
  FiChevronUp,
  FiEye,
  FiEyeOff,
} from "react-icons/fi"

export function PowerUserButtons({
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
  showDeposits: boolean
  groupingOptionsChoices: { [Key in GroupByOption]: string[] } | undefined
}) {
  const includeBudget = !!(
    groupingOptionsChoices &&
    groupingOptionsChoices[GroupByOption.budget]?.length > 1
  )

  const isMobile = useIsMobile()
  const { setCurrentFilter, currentFilter, initializeDefaultFilter } =
    useFilters()

  if (!currentFilter) {
    initializeDefaultFilter()
  }

  const handleToggleOption = (option: GroupByOption) => {
    setCurrentFilter((prev: FilterData_Input | null) => {
      if (!prev) return prev

      const newLookup = { ...prev.lookup }

        if (newLookup[option]) {
        const rest = { ...newLookup }
        delete rest[option]
        return { ...prev, lookup: rest }
        }

      const maxIndex = Object.values(newLookup).reduce(
        (max, entry) => Math.max(max, (entry as FilterEntries).index),
        -1,
      ) as number

      return {
        ...prev,
        lookup: {
          ...newLookup,
          [option]: {
            all: true,
            visible: true,
            specifics: [],
            index: maxIndex + 1,
          },
        },
      }
    })
  }

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor),
  )

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
          onDragEnd={(event) =>
            handleDragEnd(event, currentFilter, setCurrentFilter)
          }
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
                items={Object.keys(currentFilter?.lookup || {}).sort(
                  (a, b) =>
                    (
                      currentFilter?.lookup?.[
                        a as GroupByOption
                      ] as FilterEntries
                    ).index -
                    (
                      currentFilter?.lookup?.[
                        b as GroupByOption
                      ] as FilterEntries
                    ).index,
                )}
                strategy={horizontalListSortingStrategy}
              >
                {Object.entries(currentFilter?.lookup || {})
                  .sort(
                    (a, b) =>
                      (a[1] as FilterEntries).index -
                      (b[1] as FilterEntries).index,
                  )
                  .map(([option, value]) => {
                    const typedOption = option as GroupByOption
                    const typedValue = value as FilterEntries
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
                          moveItemUp={(option) =>
                            moveItemUp(option, setCurrentFilter)
                          }
                          moveItemDown={(option) =>
                            moveItemDown(option, setCurrentFilter)
                          }
                          isFirst={typedValue.index === 0}
                          isLast={
                            typedValue.index ===
                            Object.keys(currentFilter?.lookup || {}).length - 1
                          }
                          choices={groupingOptionsChoices?.[typedOption]}
                          option={typedOption}
                          noX={
                            Object.keys(currentFilter?.lookup || {}).length ===
                            1
                          }
                          onRemove={handleToggleOption}
                        />
                      </Flex>
                    )
                  })}
              </SortableContext>
              <Box
                key="blah"
                paddingLeft={
                  Object.keys(currentFilter?.lookup || {}).length * 2
                }
              >
                <GroupingConfig
                  showBudgets={includeBudget}
                  groupingOptions={Object.keys(currentFilter?.lookup || {}).map(
                    (key) => key as GroupByOption,
                  )}
                />
              </Box>
            </Flex>
          </Flex>
        </DndContext>
      </Box>
    </>
  )
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
  option: GroupByOption
  choices: string[] | undefined
  noX: boolean
  onRemove: (option: GroupByOption) => void
  moveItemUp: (option: GroupByOption) => void
  moveItemDown: (option: GroupByOption) => void
  isFirst: boolean
  isLast: boolean
}) => {
  const { attributes, listeners, setNodeRef, transform, transition } =
    useSortable({
      id: option,
    })

  const { currentFilter, setCurrentFilter } = useFilters()

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  }

  const isVisible = currentFilter?.lookup?.[option]?.visible ?? true

  const toggleVisibility = () => {
    if (!currentFilter?.lookup) return

    setCurrentFilter((prev) => {
      if (!prev?.lookup) return prev

      return {
        ...prev,
        lookup: {
          ...prev.lookup,
          [option]: {
            ...prev.lookup[option],
            visible: !isVisible,
          },
        },
      }
    })
  }

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
  )
}

interface FilterButtonProps {
  options: string[]
  name: string
}

export function FilterButton({ name, options }: FilterButtonProps) {
  const { currentFilter, setCurrentFilter } = useFilters()

  if (!currentFilter) {
    return null
  }

  const handleToggle = (option: string) => {
    if (!currentFilter.lookup) {
      return
    }

    const updatedFilter = { ...currentFilter }

    const currentValues = updatedFilter.lookup?.[name]?.specifics || []

    if (currentValues.some((value) => value.value === option)) {
      updatedFilter.lookup = {
        ...updatedFilter.lookup,
        [name]: {
          specifics: currentValues.filter((value) => value.value !== option),
          visible: true,
          index: currentFilter.lookup[name].index,
        },
      }
    } else {
      updatedFilter.lookup = {
        ...updatedFilter.lookup,
        [name]: {
          specifics: [...currentValues, { value: option }],
          visible: true,
          index: currentFilter.lookup[name].index,
        },
      }
    }

    setCurrentFilter(updatedFilter)
  }

  const handleSelectAll = () => {
    if (!currentFilter.lookup) {
      return
    }

    const updatedFilter = { ...currentFilter }

    updatedFilter.lookup = {
      ...updatedFilter.lookup,
      [name]: {
        specifics: options.map((option) => ({ value: option })),
        visible: true,
        index: currentFilter.lookup[name].index,
      },
    }

    setCurrentFilter(updatedFilter)
  }

  const handleUnselectAll = () => {
    if (!currentFilter.lookup) {
      return
    }

    const updatedFilter = { ...currentFilter }

    updatedFilter.lookup = {
      ...updatedFilter.lookup,
      [name]: {
        specifics: [],
        visible: true,
        index: currentFilter.lookup[name].index,
      },
    }

    setCurrentFilter(updatedFilter)
  }

  const currentValues = currentFilter.lookup?.[name]?.specifics || []
  const hasApplied = currentValues.length > 0

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
                  .includes(option)
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
                )
              })}
            </Menu.ItemGroup>
          </Menu.Content>
        </Menu.Positioner>
      </Portal>
    </Menu.Root>
  )
}
