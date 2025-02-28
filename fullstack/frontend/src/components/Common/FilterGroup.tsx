import type React from "react"
import { useState } from "react"

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
import type { TransactionSourceGroup } from "../../client"
import BoxWithText, { type CollapsibleName } from "./BoxWithText"
import { type GroupByOption, GroupingConfig } from "./GroupingConfig"
import { TransactionSourceSelector } from "./TransactionSourceSelector"
import { WithdrawDepositSelector } from "./WithdrawDepositSelector"

import { CSS } from "@dnd-kit/utilities"

import type { TransactionsGetAggregatedTransactionsResponse } from "../../client"

export function FilterGroup({
  activeTransactionSource,
  setActiveTransactionSource,
  data,
  groupingOptions,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  activeTransactionSource: TransactionSourceGroup
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
  showDeposits: boolean
  setActiveTransactionSource: React.Dispatch<
    React.SetStateAction<TransactionSourceGroup | null>
  >
  data: TransactionsGetAggregatedTransactionsResponse
  groupingOptions: GroupByOption[]
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
        backgroundColor: "black",
        zIndex: 100,
        minHeight: "150px",
        padding: "1px 0",
        marginBottom: "10px",
      }}
    >
      <div style={{ paddingTop: "10px", alignItems: "center", justifyContent: "center" }}>
        <BoxWithText
          text="Filters"
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
          isCollapsable={false}
          COMPONENT_NAME="Filters"
        >
          <HStack gap={4} paddingTop={4} justify={"space-between"}>
            <WithdrawDepositSelector
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
            />
            <TransactionSourceSelector
              activeTransactionSource={activeTransactionSource}
              setActiveTransactionSource={setActiveTransactionSource}
              allTransactionSources={data.groups}
            />
            <GroupingConfig
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
            />
          </HStack>
          <Box mt={4} backgroundColor={"gray.900"} borderRadius="lg">
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
                <Text>from</Text>
                <Tag.Root
                  paddingY={1.5}
                  paddingX={2}
                  color="green.500"
                  size="lg"
                  cursor="default"
                >
                  <Text>{activeTransactionSource.transaction_source_name}</Text>
                </Tag.Root>
                <Text>grouped by</Text>
                <SortableContext
                  items={groupingOptions}
                  strategy={horizontalListSortingStrategy}
                >
                  {groupingOptions.map((option, index) => (
                    <SortableItem
                      key={option}
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
}: {
  option: GroupByOption
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
          <CloseButton onClick={() => onRemove(option)} ml={2} size="sm" />
        )}
      </Tag.Root>
      {children}
    </>
  )
}
