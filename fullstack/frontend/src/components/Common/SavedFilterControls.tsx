import { useState, useRef } from 'react';
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
import { useFilters } from "@/contexts/FilterContext";
import { FiBookmark,  FiSave } from 'react-icons/fi'
import { SavedFilter } from "@/client"



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

export function SavedFilterControls() {
  const navigate = useNavigate();
  const {
    savedFilters,
    setCurrentFilter,
    saveCurrentFilter,
  } = useFilters();

  const [isLoadDialogOpen, setIsLoadDialogOpen] = useState(false);
  const [isSaveDialogOpen, setIsSaveDialogOpen] = useState(false);
 //TODO names 
  const [filterName, setFilterName] = useState('');
  const [filterDescription, setFilterDescription] = useState('');
  const [selectedFilterId, setSelectedFilterId] = useState<number | null>(null);
  
  const cancelRef = useRef<HTMLButtonElement | null>(null);

  const handleSaveFilter = () => {
    if (!filterName.trim()) return;
    
    const newFilter = {
      name: filterName,
      description: filterDescription || null,
    };
    saveCurrentFilter(newFilter);
    setIsSaveDialogOpen(false);
  };


  const handleLoadFilter = () => {
    if (!selectedFilterId) return;
    
    const filter = [...savedFilters].find(f => f.id === selectedFilterId);
    if (!filter) return;
    
    setCurrentFilter(filter.filter_data);
    setIsLoadDialogOpen(false);
    
    navigate({
      search: (prev: Record<string, unknown>) => ({ ...prev, filter: filter?.name }),
      replace: true,
    });
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


  return (
    <Flex gap={2}>
      {/* Load Filter Button */}
      <Button size="sm" variant="outline" onClick={() => setIsLoadDialogOpen(true)}>
        <FiBookmark style={{ marginRight: '0.5rem' }} />
        Load Filter
      </Button>

      {/* Save Filter Button */}
      <Button size="sm" variant="outline" onClick={() => setIsSaveDialogOpen(true)}>
        <FiSave style={{ marginRight: '0.5rem' }} />
        Save Filter
      </Button>


      {/* Load Filter Dialog */}
      <DialogRoot open={isLoadDialogOpen} onOpenChange={handleLoadDialogOpenChange}>
        <DialogContent>
          <DialogHeader>Load Saved Filter</DialogHeader>
          <DialogBody>
            <FieldRoot>
              <FieldLabel>Select a filter</FieldLabel>
              <SelectRoot
                value={[selectedFilterId?.toString() || '']}
                collection={createListCollection({
                  items: [...myFilters.items]
                })}
                onValueChange={(value) => setSelectedFilterId(Number(value))}
              >
                <SelectTrigger>
                  <SelectValueText placeholder="Select a filter" />
                </SelectTrigger>
                <SelectContent>
                      {myFilters.items.map((item) => (
                        <SelectItem key={item.value} item={item}>
                          <Box>
                            <Box fontWeight="medium">{item.label}</Box>
                            {item.description && (
                              <Box fontSize="sm" color="gray.500">
                                {item.description}
                              </Box>
                            )}
                          </Box>
                        </SelectItem>
                      ))}
                </SelectContent>
              </SelectRoot>
            </FieldRoot>
          </DialogBody>
          <DialogFooter>
            <Button ref={cancelRef} onClick={() => setIsLoadDialogOpen(false)}>
              Cancel
            </Button>
            <Button colorScheme="blue" onClick={handleLoadFilter} ml={3}>
              Load
            </Button>
          </DialogFooter>
        </DialogContent>
      </DialogRoot>

      {/* Save Filter Dialog */}
      <DialogRoot open={isSaveDialogOpen} onOpenChange={handleSaveDialogOpenChange}>
        <DialogContent>
          <DialogHeader>Save Filter</DialogHeader>
          <DialogBody>
            <FieldRoot mb={4}>
              <FieldLabel>Filter Name</FieldLabel>
              <Input
                value={filterName}
                onChange={(e) => setFilterName(e.target.value)}
                placeholder="Enter a name for your filter"
              />
            </FieldRoot>
            
            <FieldRoot>
              <FieldLabel>Description (optional)</FieldLabel>
              <Textarea
                value={filterDescription || ''}
                onChange={(e) => setFilterDescription(e.target.value)}
                placeholder="Enter a description for your filter"
              />
            </FieldRoot>
          </DialogBody>
          <DialogFooter>
            <Button ref={cancelRef} onClick={() => setIsSaveDialogOpen(false)}>
              Cancel
            </Button>
            <Button colorScheme="blue" onClick={handleSaveFilter} ml={3}>
              Save
            </Button>
          </DialogFooter>
        </DialogContent>
      </DialogRoot>
    </Flex>
  );
}
