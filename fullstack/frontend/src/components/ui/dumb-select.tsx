import { Select, Portal, createListCollection } from "@chakra-ui/react";
import { useEffect, useState } from "react";

interface DumbSelectProps<T> {
  selectedOption: T | null;
  setSelectedOption: (value: T) => void;
  options: T[];
  labelExtractor: (value: T) => string;
  keyExtractor: (value: T) => string;
  placeholder?: string;
  label?:string;
}


interface CollectionItem<T>{
    label: string;
    value: string;
    option: T;
}

function makeCollection<T>(options: T[], labelExtractor: (value: T) => string, keyExtractor: (value: T) => string) {
    const items: CollectionItem<T>[] =  options.map((option) => ({
      label: labelExtractor(option),
      value: keyExtractor(option),
      option: option,
    }))
    return createListCollection({
      items,
    })
}

export function DumbSelect<T>({
  selectedOption,
  setSelectedOption,
  options,
  labelExtractor,
  keyExtractor,
  placeholder = "Select an option",
  label = "Select an option",
}: DumbSelectProps<T>) {

  const [value, setValue] = useState<string[]>([keyExtractor(selectedOption ?? options[0])])
  const collection = makeCollection(options, labelExtractor, keyExtractor)

  useEffect(() => {
    if (value.length > 0) {
      const option = collection.find(value[0])?.option
      if (option) {
        setSelectedOption(option)
      }
    }
  }, [value])

  return (
    <Select.Root
      collection={collection}
      width="320px"
      value={value}
      onValueChange={(e) => setValue(e.value)}
    >
      <Select.HiddenSelect />
      <Select.Label>{label}</Select.Label>
      <Select.Control>
        <Select.Trigger>
          <Select.ValueText placeholder={placeholder} />
        </Select.Trigger>
        <Select.IndicatorGroup>
          <Select.Indicator />
        </Select.IndicatorGroup>
      </Select.Control>
      <Portal>
        <Select.Positioner>
          <Select.Content>
            {collection.items.map((item) => (
              <Select.Item item={item} key={item.value}>
                {item.label}
                <Select.ItemIndicator />
              </Select.Item>
            ))}
          </Select.Content>
        </Select.Positioner>
      </Portal>
    </Select.Root>
  ) 
}
