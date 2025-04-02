import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import useCustomToast from './useCustomToast';
import { SavedFiltersService } from '@/client';

export interface SavedFilter {
  id: number;
  name: string;
  description?: string;
  filter_data: Record<string, unknown>;
  is_public: boolean;
  created_at: string;
  updated_at: string;
  user_id: number;
}

export interface FilterData {
  years?: string[];
  accounts?: string[];
  months?: string[];
  categories?: string[];
  budgets?: string[];
}

export function useSavedFilters() {
  const toast = useCustomToast();
  const queryClient = useQueryClient();
  const [currentFilter, setCurrentFilter] = useState<SavedFilter | null>(null);

  // Fetch all user's saved filters
  const { data: savedFilters = [], isLoading } = useQuery({
    queryKey: ['saved-filters'],
    queryFn: () => SavedFiltersService.readSavedFilters(),
  });

  // Fetch public filters
  const { data: publicFilters = [] } = useQuery({
    queryKey: ['public-filters'],
    queryFn: () => SavedFiltersService.readPublicSavedFilters(),
  });

  // Create a new saved filter
  const createFilterMutation = useMutation({
    mutationFn: (filterData: { 
      name: string; 
      description?: string; 
      filter_data: FilterData; 
      is_public: boolean;
    }) => SavedFiltersService.createSavedFilter({requestBody: filterData}),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['saved-filters'] });
      toast(
        'Filter saved',
        'Your filter has been saved successfully.',
        'success',
      );
    },
    onError: (error) => {
      toast(
        'Error saving filter',
        'There was an error saving your filter. Please try again.',
        'error',
      );
    },
  });

  // Update an existing saved filter
  const updateFilterMutation = useMutation({
    mutationFn: ({ 
      id, 
      data 
    }: { 
      id: number; 
      data: Partial<{ 
        name: string; 
        description?: string; 
        filter_data: FilterData; 
        is_public: boolean;
      }> 
    }) => SavedFiltersService.updateSavedFilter({filterId: id, requestBody: data}),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['saved-filters'] });
      toast(
        'Filter updated',
        'Your filter has been updated successfully.',
        'success',
      );
    },
    onError: (error) => {
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
    onError: (error) => {
      toast(
        'Error deleting filter',
        'There was an error deleting your filter. Please try again.',
        'error',
      );
    },
  });

  // Load a filter by name (for URL sharing)
  const loadFilterByName = async (name: string): Promise<FilterData | null> => {
    try {
      const filter = await SavedFiltersService.readSavedFilterByName({filterName: name});
      setCurrentFilter(filter);
      return filter.filter_data;
    } catch (error) {
      toast(
        'Error loading filter',
        `Could not load filter "${name}". It may not exist or you don't have access to it.`,
        'error',
      );
      return null;
    }
  };

  // Save current filter state
  const saveCurrentFilter = (
    name: string, 
    filterData: FilterData, 
    options?: { description?: string; isPublic?: boolean }
  ) => {
    if (currentFilter) {
      // Update existing filter
      updateFilterMutation.mutate({
        id: currentFilter.id,
        data: {
          name,
          description: options?.description,
          filter_data: filterData,
          is_public: options?.isPublic ?? currentFilter.is_public,
        },
      });
    } else {
      // Create new filter
      createFilterMutation.mutate({
        name,
        description: options?.description,
        filter_data: filterData,
        is_public: options?.isPublic ?? false,
      });
    }
  };

  return {
    savedFilters,
    publicFilters,
    isLoading,
    currentFilter,
    setCurrentFilter,
    saveCurrentFilter,
    loadFilterByName,
    deleteFilter: deleteFilterMutation.mutate,
  };
}
