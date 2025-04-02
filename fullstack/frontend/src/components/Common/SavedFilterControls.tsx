import { useState, useRef, ChangeEvent } from 'react';
import { useNavigate } from '@tanstack/react-router';
import {
  Box,
  Button,
  FieldLabel,
  FieldRoot,
  Flex,
  Input,
  Textarea,
  SelectContent,
  SelectItem,
  SelectItemGroup,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
} from "@chakra-ui/react"
import {
  DialogRoot,
  DialogContent,
  DialogHeader,
  DialogBody,
  DialogFooter,
} from "@/components/ui/dialog"
import { FilterInfo } from './FilterGroup'
import { useSavedFilters, FilterData } from '@/hooks/useSavedFilters'
import { FiBookmark, FiShare2, FiSave } from 'react-icons/fi'
import { SavedFilter } from "@/client"

interface SavedFilterControlsProps {
  filterInfo: FilterInfo;
}

interface FilterSelectItem {
  label: string;
  value: string;
  description: string | null | undefined;
}

function formatFiltersForSelect(filters: SavedFilter[]): { items: FilterSelectItem[] } {
  return {
    items: filters.map((filter) => ({
      label: filter.name,
      value: filter.id.toString(),
      description: filter.description,
    }))
  };
}

export function SavedFilterControls({ filterInfo }: SavedFilterControlsProps) {
  const navigate = useNavigate();
  const {
    savedFilters,
    publicFilters,
    currentFilter,
    setCurrentFilter,
    saveCurrentFilter,
  } = useSavedFilters();

  const [isLoadDialogOpen, setIsLoadDialogOpen] = useState(false);
  const [isSaveDialogOpen, setIsSaveDialogOpen] = useState(false);
  
  const [filterName, setFilterName] = useState(currentFilter?.name || '');
  const [filterDescription, setFilterDescription] = useState(currentFilter?.description || '');
  const [selectedFilterId, setSelectedFilterId] = useState<number | null>(null);
  
  const cancelRef = useRef<HTMLButtonElement | null>(null);

  // Get current filter data
  const getCurrentFilterData = (): FilterData => {
    return {
      years: filterInfo.years,
      accounts: filterInfo.accounts,
      months: filterInfo.months,
      categories: filterInfo.categories,
      budgets: filterInfo.budgets,
    };
  };

  // Handle saving a filter
  const handleSaveFilter = () => {
    if (!filterName.trim()) return;
    
    saveCurrentFilter(filterName, getCurrentFilterData(), {
      description: filterDescription,
    });
    
    setIsSaveDialogOpen(false);
  };

  // Handle loading a filter
  const handleLoadFilter = () => {
    if (!selectedFilterId) return;
    
    const filter: SavedFilter | undefined = [...savedFilters, ...publicFilters].find(f => f.id === selectedFilterId);
    if (!filter) return;
    
    setCurrentFilter(filter);
    
    // Apply the filter data to the current filter state
    const { filter_data } = filter;
    if (filter_data.years) filterInfo.setYears(filter_data.years as string[]);
    if (filter_data.accounts) filterInfo.setAccounts(filter_data.accounts as string[]);
    if (filter_data.months) filterInfo.setMonths(filter_data.months as string[]);
    if (filter_data.categories) filterInfo.setCategories(filter_data.categories as string[]);
    if (filter_data.budgets) filterInfo.setBudgets(filter_data.budgets as string[]);
    
    setIsLoadDialogOpen(false);
    
    // Update the URL to include the filter name
    navigate({
      search: (prev: Record<string, unknown>) => ({ ...prev, filter: filter.name }),
      replace: true,
    });
  };

  // Handle sharing a filter
  const handleShareFilter = () => {
    if (!currentFilter) return;
    
    // Create a URL with the filter name
    const url = new URL(window.location.href);
    url.searchParams.set('filter', currentFilter.name);
    
    // Copy to clipboard
    navigator.clipboard.writeText(url.toString());
  };

  // Handle dialog open state changes
  const handleLoadDialogOpenChange = () => {
    setIsLoadDialogOpen(!isLoadDialogOpen);
  };

  const handleSaveDialogOpenChange = () => {
    setIsSaveDialogOpen(!isSaveDialogOpen);
  };

  // Format filters for select component
  const myFilters = formatFiltersForSelect(savedFilters);
  const otherFilters = formatFiltersForSelect(
    publicFilters.filter(f => !savedFilters.some(sf => sf.id === f.id))
  );

  return (
    <Flex gap={2}>
      {/* Load Filter Button */}
      <Button size="sm" variant="outline" onClick={() => setIsLoadDialogOpen(true)}>
        <FiBookmark style={{ marginRight: '0.5rem' }} />
        Load Filter
      </Button>
      
      <DialogRoot open={isLoadDialogOpen} onOpenChange={handleLoadDialogOpenChange}>
        <DialogContent>
          <DialogHeader>Load Saved Filter</DialogHeader>
          <DialogBody>
            <FieldRoot>
              <FieldLabel htmlFor="filter-select">Saved Filters</FieldLabel>
              <SelectRoot
                id="filter-select"
                placeholder="Select a filter"
                value={selectedFilterId ? [selectedFilterId.toString()] : []}
                collection={createListCollection({
                  items: [...myFilters.items, ...otherFilters.items]
                })}
                onValueChange={(val) => setSelectedFilterId(Number(val.value[0]))}
              >
                <SelectTrigger>
                  <SelectValueText placeholder="Select a filter" />
                </SelectTrigger>
                <SelectContent>
                  {myFilters.items.length > 0 && (
                    <SelectItemGroup title="My Filters">
                      {myFilters.items.map((filter) => (
                        <SelectItem key={filter.value} item={filter}>
                          {filter.label}
                        </SelectItem>
                      ))}
                    </SelectItemGroup>
                  )}
                  
                  {otherFilters.items.length > 0 && (
                    <SelectItemGroup title="Public Filters">
                      {otherFilters.items.map((filter) => (
                        <SelectItem key={filter.value} item={filter}>
                          {filter.label}
                        </SelectItem>
                      ))}
                    </SelectItemGroup>
                  )}
                </SelectContent>
              </SelectRoot>
            
            {selectedFilterId && (
              <Box>
                <FieldLabel>Description</FieldLabel>
                <Box p={2} bg="gray.100" borderRadius="md" fontSize="sm">
                  {[...savedFilters, ...publicFilters].find(f => f.id === selectedFilterId)?.description || 'No description provided.'}
                </Box>
              </Box>
            )}

            </FieldRoot>
          </DialogBody>
          
          <DialogFooter>
            <Button variant="outline" mr={3} onClick={() => setIsLoadDialogOpen(false)} ref={cancelRef}>
              Cancel
            </Button>
            <Button colorScheme="blue" onClick={handleLoadFilter} disabled={!selectedFilterId}>
              Load Filter
            </Button>
          </DialogFooter>
        </DialogContent>
      </DialogRoot>
      
      {/* Save Filter Button */}
      <Button size="sm" variant="outline" onClick={() => setIsSaveDialogOpen(true)}>
        <FiSave style={{ marginRight: '0.5rem' }} />
        Save Filter
      </Button>
      
      <DialogRoot open={isSaveDialogOpen} onOpenChange={handleSaveDialogOpenChange}>
        <DialogContent>
          <DialogHeader>Save Filter</DialogHeader>
          <DialogBody>
            <FieldRoot mb={4}>
              <FieldLabel htmlFor="filter-name">Filter Name</FieldLabel>
              <Input
                id="filter-name"
                value={filterName}
                onChange={(e: ChangeEvent<HTMLInputElement>) => setFilterName(e.target.value)}
                placeholder="Enter a name for your filter"
              />
            </FieldRoot>
            
            <FieldRoot mb={4}>
              <FieldLabel htmlFor="filter-description">Description (optional)</FieldLabel>
              <Textarea
                id="filter-description"
                value={filterDescription}
                onChange={(e: ChangeEvent<HTMLTextAreaElement>) => setFilterDescription(e.target.value)}
                placeholder="Describe what this filter shows"
                rows={3}
              />
            </FieldRoot>
            
          </DialogBody>
          
          <DialogFooter>
            <Button variant="outline" mr={3} onClick={() => setIsSaveDialogOpen(false)} ref={cancelRef}>
              Cancel
            </Button>
            <Button colorScheme="blue" onClick={handleSaveFilter} disabled={!filterName.trim()}>
              Save Filter
            </Button>
          </DialogFooter>
        </DialogContent>
      </DialogRoot>
      
      {/* Share Filter Button (only visible if there's a current filter) */}
      {currentFilter && (
        <Button size="sm" variant="outline" onClick={handleShareFilter}>
          <FiShare2 style={{ marginRight: '0.5rem' }} />
          Share
        </Button>
      )}
    </Flex>
  );
}
