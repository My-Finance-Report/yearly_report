import type React from "react"
import { Checkbox, CheckboxGroup, Fieldset, For } from "@chakra-ui/react"
import {
  PopoverBody,
  PopoverContent,
  PopoverRoot,
  PopoverTitle,
  PopoverTrigger,
} from "@/components/ui/popover"

import { Box, CloseButton, HStack, Tag, Text } from "@chakra-ui/react"
import {
  DndContext,
  type DragEndEvent,
  KeyboardSensor,
  PointerSensor,
  closestCenter,
  useSensor,
  useSensors,
} from "@dnd-kit/core"
import {
  SortableContext,
  arrayMove,
  horizontalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable"
import BoxWithText, { type CollapsibleName } from "./BoxWithText"
import { type GroupByOption, GroupingConfig } from "./GroupingConfig"
import { WithdrawDepositSelector } from "./WithdrawDepositSelector"

import { CSS } from "@dnd-kit/utilities"
import { BsFunnel } from "react-icons/bs"


export function FilterGroup({
  groupingOptions,
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
  showDeposits: boolean
  groupingOptions: GroupByOption[]
  groupingOptionsChoices: { [key in GroupByOption]: string[] }
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  collapsedItems: CollapsibleName[]
}) {
  const handleToggleOption = (option: GroupByOption) => {
    setGroupingOptions((prev: GroupByOption[]) => {
      return prev.includes(option)
        ? prev.length > 1
          ? prev.filter((o) => o !== option)
          : prev
        : [...prev, option]
    })
  }

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor),
  )

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event
    if (!over || active.id === over.id) return

    const oldIndex = groupingOptions.indexOf(active.id as GroupByOption)
    const newIndex = groupingOptions.indexOf(over.id as GroupByOption)
    setGroupingOptions(arrayMove(groupingOptions, oldIndex, newIndex))
  }

  return (
    <div
      style={{
        position: "sticky",
        top: 80,
        backgroundColor: "background",
        zIndex: 100,
        minHeight: "150px",
        padding: "1px 0",
        marginBottom: "10px",
      }}
    >
      <div
        style={{
          paddingTop: "10px",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <BoxWithText
          text="Filters"
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
          isCollapsable={false}
          COMPONENT_NAME="Filters"
        >
          <HStack gap={4} paddingTop={4}>
            <WithdrawDepositSelector
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
            />
            <GroupingConfig
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
            />
          </HStack>
          <Box mt={4} backgroundColor={"background"} borderRadius="lg">
            <DndContext
              sensors={sensors}
              collisionDetection={closestCenter}
              onDragEnd={handleDragEnd}
            >
              <HStack p={4} justifyContent={"center"}>
                <Text>Showing</Text>
                <Tag.Root
                  paddingY={1.5}
                  color="blue.500"
                  paddingX={2}
                  size="lg"
                  cursor="default"
                >
                  <Text>{showDeposits ? "Deposits" : "Withdrawals"}</Text>
                </Tag.Root>
                <Text>grouped by</Text>
                <SortableContext
                  items={groupingOptions}
                  strategy={horizontalListSortingStrategy}
                >
                  {groupingOptions.map((option, index) => (
                    <SortableItem
                      key={option}
                      choices={groupingOptionsChoices[option]}
                      option={option}
                      noX={groupingOptions.length === 1}
                      onRemove={handleToggleOption}
                    >
                      {index !== groupingOptions.length - 1 && (
                        <Text>then</Text>
                      )}
                    </SortableItem>
                  ))}
                </SortableContext>
              </HStack>
            </DndContext>
          </Box>
        </BoxWithText>
      </div>
    </div>
  )
}

const SortableItem = ({
  option,
  onRemove,
  noX,
  children,
  choices
}: {
  option: GroupByOption
  choices: string[]
  noX: boolean
  onRemove: (option: GroupByOption) => void
  children: React.ReactNode
}) => {
  const { attributes, listeners, setNodeRef, transform, transition } =
    useSortable({
      id: option,
    })

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  }


  return (
    <>
      <Tag.Root
        ref={setNodeRef}
        py={noX ? 2 : 0}
        color="orange.500"
        px={2}
        style={style}
        {...attributes}
        {...listeners}
      >
        <Text cursor="grab">
          {option.charAt(0).toUpperCase() + option.slice(1)}
        </Text>
        {!noX && (
          <>
            <FilterButton options={choices} name={"testing"} />
            <CloseButton onClick={() => onRemove(option)} ml={2} size="sm" />
          </>
        )}
      </Tag.Root>
      {children}
    </>
  )
}


function FilterButton({options, name}: {options: string[], name: string}) {
  return (
    <PopoverRoot>
      <PopoverTrigger >
        < BsFunnel />
      </PopoverTrigger>
      <PopoverContent>
        <PopoverBody>
          <PopoverTitle />
   <Fieldset.Root>
      <CheckboxGroup defaultValue={options} name={name}>
        <Fieldset.Legend fontSize="sm" mb="2">
        </Fieldset.Legend>
        <Fieldset.Content>
          <For each={options}>
            {(value) => (
              <Checkbox.Root key={value} value={value}>
                <Checkbox.HiddenInput />
                <Checkbox.Control>
                  <Checkbox.Indicator />
                </Checkbox.Control>
                <Checkbox.Label>{value}</Checkbox.Label>
              </Checkbox.Root>
            )}
          </For>
        </Fieldset.Content>
      </CheckboxGroup>
    </Fieldset.Root>         
        </PopoverBody>
      </PopoverContent>
    </PopoverRoot>
  )
}