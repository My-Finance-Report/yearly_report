import {
  Box,
  Tag,
  TagLabel,
  HStack,
  Text,
  CloseButton,
  Icon,
} from "@chakra-ui/react";
import { AddIcon } from "@chakra-ui/icons";
import {
  DndContext,
  closestCenter,
  useSensor,
  useSensors,
  PointerSensor,
  KeyboardSensor,
} from "@dnd-kit/core";
import {
  SortableContext,
  useSortable,
  arrayMove,
  horizontalListSortingStrategy,
} from "@dnd-kit/sortable";
import { CSS } from "@dnd-kit/utilities";

export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

interface GroupingConfigProps {
  groupingOptions: GroupByOption[];
  setGroupingOptions: (options: GroupByOption[]) => void;
  availableOptions: GroupByOption[];
}

export function GroupingConfig({
  groupingOptions,
  setGroupingOptions,
  availableOptions,
}: GroupingConfigProps) {
  const handleToggleOption = (option: GroupByOption) => {
    setGroupingOptions((prev) =>
      prev.includes(option)
        ? prev.length > 1
          ? prev.filter((o) => o !== option)
          : prev
        : [...prev, option]
    );
  };

  const sensors = useSensors(
    useSensor(PointerSensor, { activationConstraint: { distance: 5 } }),
    useSensor(KeyboardSensor)
  );

  const handleDragEnd = (event: any) => {
    const { active, over } = event;
    if (!over || active.id === over.id) return;

    const oldIndex = groupingOptions.indexOf(active.id as GroupByOption);
    const newIndex = groupingOptions.indexOf(over.id as GroupByOption);
    setGroupingOptions(arrayMove(groupingOptions, oldIndex, newIndex));
  };

  return (
    <Box p={4} borderWidth={1} borderRadius="md">
      <DndContext sensors={sensors} collisionDetection={closestCenter} onDragEnd={handleDragEnd}>

        <HStack spaceX={2} wrap="nowrap">
            {availableOptions.map((option) => (
            <Tag.Root
              key={option}
              colorScheme="blue"
              cursor={groupingOptions.includes(option) ? "not-allowed" : "pointer"}
              opacity={groupingOptions.includes(option) ? 0.5 : 1}
              p={2}
              borderRadius="md"
              onClick={() => !groupingOptions.includes(option) && handleToggleOption(option)}
            >
              <Icon as={AddIcon} mr={1} />
              <TagLabel>
                {option.charAt(0).toUpperCase() + option.slice(1)}
              </TagLabel>
            </Tag.Root>
          ))}
            <Box borderRight={3} borderColor={'#ffffff'}/>

          <Text>Group by</Text>
          <SortableContext items={groupingOptions} strategy={horizontalListSortingStrategy}>
            {groupingOptions.map((option, index) => (
              <SortableItem
                key={option}
                option={option}
                noX={groupingOptions.length === 1}
                onRemove={handleToggleOption}
              >
              {index !== groupingOptions.length -1 &&
                <Text mx={1} color="gray.500">
                    then
                </Text>
              }
              </SortableItem>
            ))}
          </SortableContext>
        </HStack>
      </DndContext>
    </Box>
  );
}

// --- Sortable Draggable Tag Component ---
const SortableItem = ({
  option,
  onRemove,
  noX,
  children,
}: {
  option: GroupByOption;
  noX: boolean,
  onRemove: (option: GroupByOption) => void;
  children: any
}) => {
  const { attributes, listeners, setNodeRef, transform, transition } = useSortable({
    id: option,
  });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
  };

  return (
    <>
      <Tag.Root ref={setNodeRef} style={style} {...attributes} {...listeners}>
        <Text cursor="grab">{option.charAt(0).toUpperCase() + option.slice(1)}</Text>
        {!noX && <CloseButton onClick={() => onRemove(option)} ml={2} size="sm" />}
      </Tag.Root>
      {children}
    </>
  );
};
