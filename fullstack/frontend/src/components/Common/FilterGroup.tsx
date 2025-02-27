import React, { useState } from "react";

import { Text, Tag, HStack , CloseButton } from "@chakra-ui/react";
import { GroupByOption, GroupingConfig } from "./GroupingConfig";
import { TransactionSourceSelector } from "./TransactionSourceSelector";
import { WithdrawDepositSelector } from "./WithdrawDepositSelector";
import BoxWithText from "./BoxWithText";
import { type TransactionSourceGroup } from "../../client";
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

import { CSS } from "@dnd-kit/utilities";

import type { TransactionsGetAggregatedTransactionsResponse } from "../../client";

export function FilterGroup({
  activeTransactionSource,
  setActiveTransactionSource,
  data,
  groupingOptions,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
}: {
  activeTransactionSource: TransactionSourceGroup;
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>;
  showDeposits: boolean;
  setActiveTransactionSource: React.Dispatch<
    React.SetStateAction<TransactionSourceGroup | null>
  >;
  data: TransactionsGetAggregatedTransactionsResponse;
  groupingOptions: GroupByOption[];
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
}) {
  const [isExpanded, setIsExpanded] = useState(true);
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
    useSensor(KeyboardSensor)
  );

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;
    if (!over || active.id === over.id) return;

    const oldIndex = groupingOptions.indexOf(active.id as GroupByOption);
    const newIndex = groupingOptions.indexOf(over.id as GroupByOption);
    setGroupingOptions(arrayMove(groupingOptions, oldIndex, newIndex));
  };

  return (
    <>
      <BoxWithText
        text="Filters"
        isExpanded={isExpanded}
        setIsExpanded={setIsExpanded}
      >
        <HStack>
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
        <DndContext
          sensors={sensors}
          collisionDetection={closestCenter}
          onDragEnd={handleDragEnd}
        >
          <HStack>
            <Text>showing</Text>
            <Tag.Root paddingY={1.5} paddingX={2} size="lg" cursor="default">
              <Text>{showDeposits ? "Deposits" : "Withdrawals"}</Text>
            </Tag.Root>
            <Text>from</Text>
            <Tag.Root paddingY={1.5} paddingX={2} size="lg" cursor="default">
              <Text>{activeTransactionSource.transaction_source_name}</Text>
            </Tag.Root>
            <Text>Grouped by</Text>
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
                    <Text mx={1} color="gray.500">
                      then
                    </Text>
                  )}
                </SortableItem>
              ))}
            </SortableContext>
          </HStack>
        </DndContext>
      </BoxWithText>
    </>
  );
}

const SortableItem = ({
  option,
  onRemove,
  noX,
  children,
}: {
  option: GroupByOption;
  noX: boolean;
  onRemove: (option: GroupByOption) => void;
  children: React.ReactNode;
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
    <>
      <Tag.Root
        ref={setNodeRef}
        py={noX ? 2 : 0}
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
  );
};
