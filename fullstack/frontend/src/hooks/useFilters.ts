import  { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import useCustomToast from './useCustomToast';
import { SavedFiltersService,  SavedFilterCreate, FilterData_Input } from '@/client';

export function useFilters() {
  const toast = useCustomToast();
  const queryClient = useQueryClient();
  const [currentFilter, setCurrentFilter] = useState<FilterData_Input | null>(null);

  const { data: savedFilters = [], isLoading } = useQuery({
    queryKey: ['saved-filters'],
    queryFn: () => SavedFiltersService.readSavedFilters(),
  });

  const { data: publicFilters = [] } = useQuery({
    queryKey: ['public-filters'],
    queryFn: () => SavedFiltersService.readPublicSavedFilters(),
  });

  const createFilterMutation = useMutation({
    mutationFn: (filterData: SavedFilterCreate) => SavedFiltersService.createSavedFilter({requestBody: filterData}),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['saved-filters'] });
      toast(
        'Filter saved',
        'Your filter has been saved successfully.',
        'success',
      );
    },
    onError: () => {
      toast(
        'Error saving filter',
        'There was an error saving your filter. Please try again.',
        'error',
      );
    },
  });

  const updateFilterMutation = useMutation({
    mutationFn: ({ 
      id, 
      data 
    }: { 
      id: number; 
      data: { 
        name?: string | null; 
        description?: string | null; 
        filter_data?: FilterData_Input | null; 
      } 
    }) => SavedFiltersService.updateSavedFilter({filterId: id, requestBody: data}),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['saved-filters'] });
      toast(
        'Filter updated',
        'Your filter has been updated successfully.',
        'success',
      );
    },
    onError: () => {
      toast(
        'Error updating filter',
        'There was an error updating your filter. Please try again.',
        'error',
      );
    },
  });

  // Delete a saved filter
  const deleteFilterMutation = useMutation({
    mutationFn: (id: number) => SavedFiltersService.deleteSavedFilter({filterId: id}),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['saved-filters'] });
      if (currentFilter) {
        setCurrentFilter(null);
      }
      toast(
        'Filter deleted',
        'Your filter has been deleted successfully.',
        'success',
      );
    },
    onError: () => {
      toast(
        'Error deleting filter',
        'There was an error deleting your filter. Please try again.',
        'error',
      );
    },
  });

  const loadFilterByName = async (name: string): Promise<FilterData_Input | null> => {
    try {
      const filter = await SavedFiltersService.readSavedFilterByName({filterName: name});
      setCurrentFilter(filter.filter_data);
      return filter.filter_data;
    } catch {
      toast(
        'Error loading filter',
        `Could not load filter "${name}". It may not exist or you don't have access to it.`,
        'error',
      );
      return null;
    }
  };

  const saveCurrentFilter = (
    {name, description}: {name: string; description: string | null}
  ) => {
    if (!currentFilter) return;
    createFilterMutation.mutate({name, description, filter_data: currentFilter});
  };

  const setDefaultFilter = () => {
    // First check if there's already a current filter set
    if (currentFilter) return;
    
    // Try to find a filter marked as default in saved filters
    const defaultFilter = savedFilters.find((filter) => filter.filter_data.is_default);
    
    if (defaultFilter) {
      console.log("Setting default filter from saved filters:", defaultFilter.name);
      setCurrentFilter(defaultFilter.filter_data);
      return;
    }
    
    // If no default filter found in saved filters, check public filters
    const defaultPublicFilter = publicFilters.find((filter) => filter.filter_data.is_default);
    
    if (defaultPublicFilter) {
      console.log("Setting default filter from public filters:", defaultPublicFilter.name);
      setCurrentFilter(defaultPublicFilter.filter_data);
      return;
    }
    
    // If no default filter found at all, create a basic default filter
    console.log("No default filter found, creating a basic one");
    setCurrentFilter({
      is_default: true,
      lookup: {
        category: {
          all: true,
          visible: true,
          specifics: [],
          index: 0
        }
      }
    });
  };

  const updateFilter = (
    id: number,
    data: {
      name?: string;
      description?: string | null;
      filter_data?: FilterData_Input;
    }
  ) => {
    updateFilterMutation.mutate({ id, data });
  };

  const deleteFilter = (id: number) => {
    deleteFilterMutation.mutate(id);
  };

  return {
    savedFilters: savedFilters,
    publicFilters: publicFilters,
    currentFilter,
    setDefaultFilter,
    setCurrentFilter,
    isLoading,
    saveCurrentFilter,
    updateFilter,
    deleteFilter,
    loadFilterByName,
  };
}
