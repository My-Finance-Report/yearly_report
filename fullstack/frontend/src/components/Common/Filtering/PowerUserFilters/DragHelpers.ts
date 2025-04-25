import type {
  FilterData_Output,
  FilterEntries,
  SavedFilterOut,
} from "@/client";

import type { DragEndEvent } from "@dnd-kit/core";
import type { GroupByOption } from "@/components/Common/GroupingConfig";

export function breakoutToCustomFilter(
  filter_data: FilterData_Output,
  setCurrentFilter: React.Dispatch<React.SetStateAction<SavedFilterOut>>,
) {
  setCurrentFilter({
    name: "custom",
    id: "custom",
    description: null,
    filter_data,
  });
}

export const handleDragEnd = (
  event: DragEndEvent,
  setCurrentFilter: React.Dispatch<React.SetStateAction<SavedFilterOut>>,
  currentFilter: SavedFilterOut,
) => {
  const { active, over } = event;
  if (
    !over ||
    active.id === over.id ||
    !currentFilter ||
    !currentFilter.filter_data.lookup
  )
    return;

  const activeOption = active.id as GroupByOption;
  const overOption = over.id as GroupByOption;

  const activeIndex = currentFilter.filter_data.lookup[activeOption]?.index;
  const overIndex = currentFilter.filter_data.lookup[overOption]?.index;

  if (activeIndex === undefined || overIndex === undefined) return;

  const val: () => FilterData_Output = () => {
    if (!currentFilter.filter_data || !currentFilter.filter_data.lookup)
      return currentFilter.filter_data;

    // Create a new lookup object with swapped indices
    const newLookup = { ...currentFilter.filter_data.lookup };

    // Update all indices between activeIndex and overIndex
    Object.entries(newLookup).forEach(([option, entry]) => {
      const optionKey = option as GroupByOption;
      const filterEntry = entry as FilterEntries;

      if (activeIndex < overIndex) {
        // Moving down
        if (filterEntry.index > activeIndex && filterEntry.index <= overIndex) {
          newLookup[optionKey] = {
            ...filterEntry,
            index: filterEntry.index - 1,
          };
        }
      } else {
        // Moving up
        if (filterEntry.index >= overIndex && filterEntry.index < activeIndex) {
          newLookup[optionKey] = {
            ...filterEntry,
            index: filterEntry.index + 1,
          };
        }
      }
    });

    // Set the active option to the target index
    newLookup[activeOption] = {
      ...(newLookup[activeOption] as FilterEntries),
      index: overIndex,
    };

    return { ...currentFilter.filter_data, lookup: newLookup };
  };

  breakoutToCustomFilter(val(), setCurrentFilter);
};

export const moveItemUp = (
  option: GroupByOption,
  setCurrentFilter: React.Dispatch<React.SetStateAction<SavedFilterOut>>,
  currentFilter: SavedFilterOut,
) => {
  const generateMergedFilter: () => FilterData_Output = () => {
    if (!currentFilter.filter_data || !currentFilter.filter_data.lookup)
      return currentFilter.filter_data;

    const currentIndex = currentFilter.filter_data.lookup[option]?.index;
    if (currentIndex === undefined || currentIndex <= 0)
      return currentFilter.filter_data;

    // Find the option with index = currentIndex - 1
    const optionToSwap = Object.entries(currentFilter.filter_data.lookup).find(
      ([, entry]) => (entry as FilterEntries).index === currentIndex - 1,
    )?.[0] as GroupByOption | undefined;

    if (!optionToSwap) return currentFilter.filter_data;

    // Create a new lookup object with swapped indices
    const newLookup = { ...currentFilter.filter_data.lookup };
    newLookup[option] = {
      ...(newLookup[option] as FilterEntries),
      index: currentIndex - 1,
    };
    newLookup[optionToSwap] = {
      ...(newLookup[optionToSwap] as FilterEntries),
      index: currentIndex,
    };

    return { ...currentFilter.filter_data, lookup: newLookup };
  };
  breakoutToCustomFilter(generateMergedFilter(), setCurrentFilter);
};

export const moveItemDown = (
  option: GroupByOption,
  setCurrentFilter: React.Dispatch<React.SetStateAction<SavedFilterOut>>,
  currentFilter: SavedFilterOut,
) => {
  const generateMergedFilter: () => FilterData_Output = () => {
    if (!currentFilter.filter_data || !currentFilter.filter_data.lookup)
      return currentFilter.filter_data;

    const currentIndex = currentFilter.filter_data.lookup[option]?.index;
    if (currentIndex === undefined) return currentFilter.filter_data;

    // Find the maximum index in the lookup
    const maxIndex = Math.max(
      ...Object.values(currentFilter.filter_data.lookup).map(
        (value) => (value as FilterEntries).index,
      ),
    );

    if (currentIndex >= maxIndex) return currentFilter.filter_data;

    // Find the option with index = currentIndex + 1
    const optionToSwap = Object.entries(currentFilter.filter_data.lookup).find(
      ([, value]) => (value as FilterEntries).index === currentIndex + 1,
    )?.[0] as GroupByOption | undefined;

    if (!optionToSwap) return currentFilter.filter_data;

    // Create a new lookup object with swapped indices
    const newLookup = { ...currentFilter.filter_data.lookup };
    newLookup[option] = {
      ...(newLookup[option] as FilterEntries),
      index: currentIndex + 1,
    };
    newLookup[optionToSwap] = {
      ...(newLookup[optionToSwap] as FilterEntries),
      index: currentIndex,
    };

    return { ...currentFilter.filter_data, lookup: newLookup };
  };
  breakoutToCustomFilter(generateMergedFilter(), setCurrentFilter);
};
