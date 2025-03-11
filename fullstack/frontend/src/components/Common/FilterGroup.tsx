import type React from "react"
import {
  DrawerBackdrop,
  DrawerBody,
  DrawerCloseTrigger,
  DrawerContent,
  DrawerFooter,
  DrawerActionTrigger,
  DrawerHeader,
  DrawerRoot,
  DrawerTitle,
  DrawerTrigger,
} from "@/components/ui/drawer"


import { Button, Flex, Checkbox, CheckboxGroup, Fieldset, useDisclosure } from "@chakra-ui/react"
import {
  PopoverBody,
  PopoverContent,
  PopoverRoot,
  PopoverTrigger,
} from "@/components/ui/popover"

import { Box, CloseButton, Tag, Text } from "@chakra-ui/react"
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
import BoxWithText, { type CollapsibleName } from "./BoxWithText"
import { GroupByOption, GroupingConfig } from "./GroupingConfig"
import { WithdrawDepositSelector } from "./WithdrawDepositSelector"

import { CSS } from "@dnd-kit/utilities"
import { BsFunnel } from "react-icons/bs"
import { useEffect, useState } from "react"
import { FiChevronDown, FiChevronUp } from "react-icons/fi"
import { useRouter } from "@tanstack/react-router"
import { useIsMobile } from "@/hooks/useIsMobile"

export interface FilterInfo {
  years: string[] | null
  accounts: string[] | null
  months: string[] | null
  categories: string[] | null
  budgets: string[] | null
  setYears: React.Dispatch<React.SetStateAction<string[] | null>>
  setAccounts: React.Dispatch<React.SetStateAction<string[] | null>>
  setMonths: React.Dispatch<React.SetStateAction<string[] | null>>
  setCategories: React.Dispatch<React.SetStateAction<string[] | null>>
  setBudgets: React.Dispatch<React.SetStateAction<string[] | null>>
}

function getFilterSettings(filterInfo: FilterInfo, groupingOption: GroupByOption): [string[] | null, React.Dispatch<React.SetStateAction<string[] | null>>] {
  switch (groupingOption) {
    case GroupByOption.year:
      return [filterInfo.years, filterInfo.setYears]
    case GroupByOption.account:
      return [filterInfo.accounts, filterInfo.setAccounts]
    case GroupByOption.month:
      return [filterInfo.months, filterInfo.setMonths]
    case GroupByOption.category:
      return [filterInfo.categories, filterInfo.setCategories]
    case GroupByOption.budget:
      return [filterInfo.budgets, filterInfo.setBudgets] 
    default:
      throw "Invalid grouping option"
  }
}


export function FilterGroup({
  filterInfo,
  groupingOptionsChoices,
  groupingOptions,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
  showDeposits: boolean
  filterInfo: FilterInfo
  groupingOptions: GroupByOption[]
  groupingOptionsChoices: { [key in GroupByOption]: string[] }
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  collapsedItems: CollapsibleName[]
}) {

  const isMobile = useIsMobile()


  const [open, setOpen] = useState(false)

  if (isMobile) {
    return (
      <DrawerRoot open={open} onOpenChange={(e) => setOpen(e.open)} placement={'bottom'}>
        <DrawerBackdrop />
        <DrawerTrigger asChild>
          <Button variant="outline" size="sm">
            <Text>Adjust Filters</Text>
            <BsFunnel />
          </Button>
        </DrawerTrigger>
        <DrawerContent>
          <DrawerHeader>
            <DrawerTitle>Filters</DrawerTitle>
          </DrawerHeader>
          <DrawerBody>
            <InnerFilterGroup
              filterInfo={filterInfo}
              groupingOptionsChoices={groupingOptionsChoices}
              groupingOptions={groupingOptions}
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
              setGroupingOptions={setGroupingOptions}
              setCollapsedItems={setCollapsedItems}
              collapsedItems={collapsedItems}
            />
          </DrawerBody>
          <DrawerFooter>
            <DrawerActionTrigger asChild>
              <Button variant="outline">Close</Button>
            </DrawerActionTrigger>
          </DrawerFooter>
          <DrawerCloseTrigger />
        </DrawerContent>
      </DrawerRoot>
    )
  }



  return <InnerFilterGroup
    filterInfo={filterInfo}
    groupingOptionsChoices={groupingOptionsChoices}
    groupingOptions={groupingOptions}
    setShowDeposits={setShowDeposits}
    showDeposits={showDeposits}
    setGroupingOptions={setGroupingOptions}
    setCollapsedItems={setCollapsedItems}
    collapsedItems={collapsedItems}
  />
}


function InnerFilterGroup({
  groupingOptions,
  filterInfo,
  groupingOptionsChoices,
  setShowDeposits,
  showDeposits,
  setGroupingOptions,
  setCollapsedItems,
  collapsedItems,
}: {
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
  showDeposits: boolean
  filterInfo: FilterInfo
  groupingOptions: GroupByOption[]
  groupingOptionsChoices: { [key in GroupByOption]: string[] }
  setGroupingOptions: React.Dispatch<React.SetStateAction<GroupByOption[]>>
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
  collapsedItems: CollapsibleName[]
}) {

  const includeBudget = (filterInfo.budgets?.length || 0) > 1

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

  const moveItemUp = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      const index = prev.indexOf(option)
      if (index > 0) {
        const newOptions = [...prev]
          ;[newOptions[index - 1], newOptions[index]] = [newOptions[index], newOptions[index - 1]]
        return newOptions
      }
      return prev
    })
  }

  const moveItemDown = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      const index = prev.indexOf(option)
      if (index < prev.length - 1) {
        const newOptions = [...prev]
          ;[newOptions[index], newOptions[index + 1]] = [newOptions[index + 1], newOptions[index]]
        return newOptions
      }
      return prev
    })
  }



  const isMobile = useIsMobile()

  return (
    <div
      style={{
        position: "sticky",
        top: 80,
        backgroundColor: "background",
        zIndex: 100,
        minHeight: "150px",
        padding: "1px 0",
        marginBottom: "10px",
      }}
    >
      <div
        style={{
          paddingTop: "10px",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <BoxWithText
          text="Filters"
          setCollapsedItems={setCollapsedItems}
          collapsedItems={collapsedItems}
          isCollapsable={false}
          COMPONENT_NAME="Filters"
        >
          <Flex direction={isMobile ? "column" : "row"} gap={4} paddingTop={4} alignItems={isMobile ? "start" : "center"} justifyContent={"center"}>
            <WithdrawDepositSelector
              setShowDeposits={setShowDeposits}
              showDeposits={showDeposits}
            />
            <GroupingConfig
              showBudgets={includeBudget}
              groupingOptions={groupingOptions}
              setGroupingOptions={setGroupingOptions}
            />
          </Flex>
          <Box mt={4} backgroundColor={"background"} borderRadius="lg">
            <DndContext
              sensors={sensors}
              collisionDetection={closestCenter}
              onDragEnd={handleDragEnd}
            >
              <Flex direction={isMobile ? "column" : "row"} p={4} justifyContent={"center"} gap={2} alignItems={isMobile ? 'start' : 'center'}>
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
                <Text>grouped by</Text>
                <SortableContext
                  items={groupingOptions}
                  strategy={horizontalListSortingStrategy}
                >
                  {groupingOptions.map((option, index) => {
                    const [filters, setFilters] = getFilterSettings(filterInfo, option)
                    return (
                      <SortableItem
                        key={option}
                        filters={filters}
                        setFilters={setFilters}
                        moveItemUp={moveItemUp}
                        moveItemDown={moveItemDown}
                        isFirst={index === 0}
                        isLast={index === groupingOptions.length - 1}
                        choices={groupingOptionsChoices[option]}
                        option={option}
                        noX={groupingOptions.length === 1}
                        onRemove={handleToggleOption}
                        isMobile={!!isMobile}
                      >
                        {index !== groupingOptions.length - 1 && (
                          <Text>then</Text>
                        )}
                      </SortableItem>
                    )
                  })}
                </SortableContext>
              </Flex>
            </DndContext>
          </Box>
        </BoxWithText>
      </div>
    </div>
  )
}

const SortableItem = ({
  option,
  filters,
  setFilters,
  onRemove,
  noX,
  children,
  choices,
  moveItemUp,
  moveItemDown,
  isFirst,
  isLast,
  isMobile
}: {
  option: GroupByOption
  choices: string[]
  filters: string[] | null
  setFilters: React.Dispatch<React.SetStateAction<string[] | null>>
  noX: boolean
  onRemove: (option: GroupByOption) => void
  children: React.ReactNode
  moveItemUp: (option: GroupByOption) => void
  moveItemDown: (option: GroupByOption) => void
  isFirst: boolean
  isLast: boolean
  isMobile: boolean
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
        maxW={230}
        justifyContent="space-between"
        style={style}
        {...attributes}
        {...listeners}
      >
        <Text cursor="grab">
          {option.charAt(0).toUpperCase() + option.slice(1)}
        </Text>
        <Box p={1}>
          <FilterButton filters={filters} setFilters={setFilters} options={choices} name={option} />
          {!noX && (
            <CloseButton onClick={() => onRemove(option)} size="xs" variant="outline" />
          )}
          {isMobile &&
            (
              <>
                <Button size="xs" variant="outline" disabled={isFirst} onClick={() => moveItemUp(option)}>
                  <FiChevronUp />
                </Button>
                <Button size="xs" variant="outline" disabled={isLast} onClick={() => moveItemDown(option)}>
                  <FiChevronDown />
                </Button>
              </>
            )
          }
        </Box>
      </Tag.Root>
      {children}
    </>
  )
}
interface FilterButtonProps {
  options: string[]
  name: string
  filters: string[] | null
  setFilters: React.Dispatch<React.SetStateAction<string[] | null>>
}


function FilterButton({
  options,
  name,
  filters,
  setFilters,
}: FilterButtonProps) {
  const router = useRouter()
  const [localSelection, setLocalSelection] = useState<string[]>(
    () => filters ?? options 
  )

  useEffect(() => {
    if (filters !== null) {
      setLocalSelection(filters)
    }
  }, [filters])

  const { open, onOpen, onClose } = useDisclosure()

  const handleClose = () => {
    setFilters(localSelection)
    router.navigate({
      to: ".",
      search: (old: Record<string, string>) => ({
        ...old,
        [name]: localSelection.join(","), 
      }),
      replace: true,
  })
    
    onClose()
  }

  const handleChange = (newSelectedValues: string[]) => {
    setLocalSelection([...newSelectedValues])
  }

  const handleSelectAll = () => {
    setLocalSelection([...options])
  }

  const handleUnselectAll = () => {
    setLocalSelection([])
  }

  return (
    <PopoverRoot open={open} onOpenChange={onOpen} onExitComplete={handleClose}>
      <PopoverTrigger>
        <Button size="xs" variant="outline">
          <BsFunnel />
        </Button>
      </PopoverTrigger>

      <PopoverContent>
        <PopoverBody>
          <Fieldset.Root>
            <CheckboxGroup
              value={localSelection}
              name={name}
              onValueChange={handleChange}
            >
              <Fieldset.Content>
                {options.map((value) => (
                  <Checkbox.Root key={value} value={value}>
                    <Checkbox.HiddenInput />
                    <Checkbox.Control>
                      <Checkbox.Indicator />
                    </Checkbox.Control>
                    <Checkbox.Label>{value}</Checkbox.Label>
                  </Checkbox.Root>
                ))}
              </Fieldset.Content>
            </CheckboxGroup>
          </Fieldset.Root>
          <Flex flexDirection="row" gap={4} mt={2}>
            <Button size="xs" onClick={handleClose} >
              Apply
            </Button>
            <Button size="xs" variant="outline" onClick={handleSelectAll}>
              Select All
            </Button>
            <Button size="xs" variant="outline" onClick={handleUnselectAll}>
              Unselect All
            </Button>
</Flex>
        </PopoverBody>
      </PopoverContent>
    </PopoverRoot>
  )
}