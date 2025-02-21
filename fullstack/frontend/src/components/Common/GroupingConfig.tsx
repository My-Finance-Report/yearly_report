import { AddIcon } from "@chakra-ui/icons"
import {
  Box,
  CloseButton,
  HStack,
  Icon,
  Tag,
  TagLabel,
  Text,
} from "@chakra-ui/react"
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
  arrayMove,
  horizontalListSortingStrategy,
  useSortable,
} from "@dnd-kit/sortable"
import { CSS } from "@dnd-kit/utilities"

export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

const availableOptions: GroupByOption[] = [
  GroupByOption.category,
  GroupByOption.year,
  GroupByOption.month,
]

interface GroupingConfigProps {
  groupingOptions: GroupByOption[]
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>
}

export function GroupingConfig({
  groupingOptions,
  setGroupingOptions,
}: GroupingConfigProps) {
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

  const handleDragEnd = (event) => {
    const { active, over } = event
    if (!over || active.id === over.id) return

    const oldIndex = groupingOptions.indexOf(active.id as GroupByOption)
    const newIndex = groupingOptions.indexOf(over.id as GroupByOption)
    setGroupingOptions(arrayMove(groupingOptions, oldIndex, newIndex))
  }

  return (
    <Box p={4} borderWidth={1} borderRadius="md">
      <DndContext
        sensors={sensors}
        collisionDetection={closestCenter}
        onDragEnd={handleDragEnd}
      >
        <HStack spaceX={2} wrap="nowrap">
          {availableOptions.map((option) => (
            <Tag.Root
              key={option}
              colorScheme="blue"
              cursor={
                groupingOptions.includes(option) ? "not-allowed" : "pointer"
              }
              opacity={groupingOptions.includes(option) ? 0.5 : 1}
              p={2}
              borderRadius="md"
              onClick={() =>
                !groupingOptions.includes(option) && handleToggleOption(option)
              }
            >
              <Icon as={AddIcon} mr={1} />
              <TagLabel>
                {option.charAt(0).toUpperCase() + option.slice(1)}
              </TagLabel>
            </Tag.Root>
          ))}
          <Box borderRight={3} borderColor={"#ffffff"} />

          <Text>Group by</Text>
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
    </Box>
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
      <Tag.Root ref={setNodeRef} style={style} {...attributes} {...listeners}>
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
