import { AddIcon } from "@chakra-ui/icons"
import {
  HStack,
  Icon,
  Box,
  Tag,
  TagLabel,
} from "@chakra-ui/react"


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

  return (
    <Box border="1px solid" borderColor="gray.200" borderRadius="md" p={2}>
        <HStack spaceX={2} wrap="nowrap">
          {availableOptions.map((option) => (
            <Tag.Root
              key={option}
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
        </HStack>
</Box>
  )
}
