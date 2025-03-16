import { useCheckboxGroup, Menu, Button, Portal } from "@chakra-ui/react";
import { FiCheck } from "react-icons/fi";
import { Text } from "@chakra-ui/react";
import { AddIcon } from "@chakra-ui/icons";

export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
  account = "account",
  budget = "budget",
}

const availableOptions: GroupByOption[] = [
  GroupByOption.category,
  GroupByOption.year,
  GroupByOption.month,
  GroupByOption.account,
  GroupByOption.budget,
];

interface GroupingConfigProps {
  groupingOptions: GroupByOption[];
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>;
  showBudgets: boolean;
}

export function GroupingConfig({
  groupingOptions,
  setGroupingOptions,
  showBudgets,
}: GroupingConfigProps) {
  const handleToggleOption = (option: GroupByOption) => {
    setGroupingOptions((prev: GroupByOption[]) => {
      return prev.includes(option)
        ? prev.length > 1
          ? prev.filter((o) => o !== option)
          : prev
        : [...prev, option];
    });
  };

  const filteredOptions = showBudgets
    ? availableOptions
    : availableOptions.filter((option) => option !== GroupByOption.budget);

  const group = useCheckboxGroup({ value: groupingOptions });

  return (
    <Menu.Root key="grouping-config" onSelect={(value)=>handleToggleOption(value.value as GroupByOption)}>
      <Menu.Trigger asChild>
        <Button  variant="plain" size="sm">
          <Text textDecoration={'underline'}>Add another filter group</Text> <AddIcon/>
        </Button>
      </Menu.Trigger>
      <Portal>
        <Menu.Positioner>
          <Menu.Content zIndex={10000}>
            <Menu.ItemGroup>
              {filteredOptions.map((option, index) => (
                  <Menu.CheckboxItem
                    key={index.toString()}
                    value={option}
                    disabled={group.isChecked(option)}
                    checked={group.isChecked(option)}
                    onCheckedChange={() => group.toggleValue(option)}
                  >
                    {option.charAt(0).toUpperCase() + option.slice(1)}
                    {group.isChecked(option) && <FiCheck />}
                  </Menu.CheckboxItem>
              ))}
            </Menu.ItemGroup>
          </Menu.Content>
        </Menu.Positioner>
      </Portal>
    </Menu.Root>
  );

}
