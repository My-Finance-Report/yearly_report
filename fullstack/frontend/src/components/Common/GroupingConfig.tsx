import { useCheckboxGroup, Menu, Button, Portal } from "@chakra-ui/react";
import { FaPlus } from "react-icons/fa";
import { FiCheck } from "react-icons/fi";

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

  const group = useCheckboxGroup({ defaultValue: groupingOptions });

  return (
    <Menu.Root onSelect={(value)=>handleToggleOption(value.value as GroupByOption)}>
      <Menu.Trigger asChild>
        <Button variant="subtle" size="sm">
          Add a new filter group <FaPlus />
        </Button>
      </Menu.Trigger>
      <Portal>
        <Menu.Positioner>
          <Menu.Content zIndex={10000}>
            <Menu.ItemGroup>
              {filteredOptions.map((option) => (
                <>

                  <Menu.CheckboxItem
                    key={option}
                    value={option}
                    checked={group.isChecked(option)}
                    onCheckedChange={() => group.toggleValue(option)}
                  >
                    {option.charAt(0).toUpperCase() + option.slice(1)}
                    {group.isChecked(option) && <FiCheck />}
                  </Menu.CheckboxItem>
                </>
              ))}
            </Menu.ItemGroup>
          </Menu.Content>
        </Menu.Positioner>
      </Portal>
    </Menu.Root>
  );

}
