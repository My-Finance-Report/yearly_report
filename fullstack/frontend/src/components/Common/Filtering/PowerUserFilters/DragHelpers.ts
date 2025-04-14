import type { FilterData_Input, FilterEntries } from "@/client"

import type { DragEndEvent } from "@dnd-kit/core"
import type { GroupByOption } from "@/components/Common/GroupingConfig"

export const handleDragEnd = (
  event: DragEndEvent,
  currentFilter: FilterData_Input | null,
  setCurrentFilter: React.Dispatch<
    React.SetStateAction<FilterData_Input | null>
  >,
) => {
  const { active, over } = event
  if (!over || active.id === over.id || !currentFilter || !currentFilter.lookup)
    return

  const activeOption = active.id as GroupByOption
  const overOption = over.id as GroupByOption

  const activeIndex = currentFilter.lookup[activeOption]?.index
  const overIndex = currentFilter.lookup[overOption]?.index

  if (activeIndex === undefined || overIndex === undefined) return

  setCurrentFilter((prev: FilterData_Input | null) => {
    if (!prev || !prev.lookup) return prev

    // Create a new lookup object with swapped indices
    const newLookup = { ...prev.lookup }

    // Update all indices between activeIndex and overIndex
    Object.entries(newLookup).forEach(([option, entry]) => {
      const optionKey = option as GroupByOption
      const filterEntry = entry as FilterEntries

      if (activeIndex < overIndex) {
        // Moving down
        if (filterEntry.index > activeIndex && filterEntry.index <= overIndex) {
          newLookup[optionKey] = {
            ...filterEntry,
            index: filterEntry.index - 1,
          }
        }
      } else {
        // Moving up
        if (filterEntry.index >= overIndex && filterEntry.index < activeIndex) {
          newLookup[optionKey] = {
            ...filterEntry,
            index: filterEntry.index + 1,
          }
        }
      }
    })

    // Set the active option to the target index
    newLookup[activeOption] = {
      ...(newLookup[activeOption] as FilterEntries),
      index: overIndex,
    }

    return { ...prev, lookup: newLookup }
  })
}

export const moveItemUp = (
  option: GroupByOption,
  setCurrentFilter: React.Dispatch<
    React.SetStateAction<FilterData_Input | null>
  >,
) => {
  setCurrentFilter((prev) => {
    if (!prev || !prev.lookup) return prev

    const currentIndex = prev.lookup[option]?.index
    if (currentIndex === undefined || currentIndex <= 0) return prev

    // Find the option with index = currentIndex - 1
    const optionToSwap = Object.entries(prev.lookup).find(
      ([, entry]) => (entry as FilterEntries).index === currentIndex - 1,
    )?.[0] as GroupByOption | undefined

    if (!optionToSwap) return prev

    // Create a new lookup object with swapped indices
    const newLookup = { ...prev.lookup }
    newLookup[option] = {
      ...(newLookup[option] as FilterEntries),
      index: currentIndex - 1,
    }
    newLookup[optionToSwap] = {
      ...(newLookup[optionToSwap] as FilterEntries),
      index: currentIndex,
    }

    return { ...prev, lookup: newLookup }
  })
}

export const moveItemDown = (
  option: GroupByOption,
  setCurrentFilter: React.Dispatch<
    React.SetStateAction<FilterData_Input | null>
  >,
) => {
  setCurrentFilter((prev) => {
    if (!prev || !prev.lookup) return prev

    const currentIndex = prev.lookup[option]?.index
    if (currentIndex === undefined) return prev

    // Find the maximum index in the lookup
    const maxIndex = Math.max(
      ...Object.values(prev.lookup).map(
        (value) => (value as FilterEntries).index,
      ),
    )

    if (currentIndex >= maxIndex) return prev

    // Find the option with index = currentIndex + 1
    const optionToSwap = Object.entries(prev.lookup).find(
      ([, value]) => (value as FilterEntries).index === currentIndex + 1,
    )?.[0] as GroupByOption | undefined

    if (!optionToSwap) return prev

    // Create a new lookup object with swapped indices
    const newLookup = { ...prev.lookup }
    newLookup[option] = {
      ...(newLookup[option] as FilterEntries),
      index: currentIndex + 1,
    }
    newLookup[optionToSwap] = {
      ...(newLookup[optionToSwap] as FilterEntries),
      index: currentIndex,
    }

    return { ...prev, lookup: newLookup }
  })
}
