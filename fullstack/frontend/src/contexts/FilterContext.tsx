import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import useCustomToast from '../hooks/useCustomToast';
import { SavedFiltersService, SavedFilterCreate, FilterData_Input, SavedFilter } from '@/client';

// Define the context type
interface FilterContextType {
  // Data
  savedFilters: SavedFilter[];
  publicFilters: SavedFilter[];
  currentFilter: FilterData_Input | null;
  
  // Status
  isLoading: boolean;
  isInitialized: boolean;
  
  // Actions
  setCurrentFilter: (filter: FilterData_Input | null | ((prev: FilterData_Input | null) => FilterData_Input | null)) => void;
  initializeDefaultFilter: () => void;
  saveCurrentFilter: (data: { name: string; description: string | null }) => void;
  updateFilter: (id: number, data: { name: string; description: string | null }) => void;
  deleteFilter: (id: number) => void;
  loadFilterByName: (name: string) => Promise<FilterData_Input | null>;
}

// Create the context with a default value
const FilterContext = createContext<FilterContextType | undefined>(undefined);

// Provider component
export function FilterProvider({ children }: { children: ReactNode }) {
  const queryClient = useQueryClient();
  const toast = useCustomToast();
  
  // State for the current filter
  const [currentFilter, setCurrentFilter] = useState<FilterData_Input | null>(null);
  
  // Track if we've attempted to initialize the filter
  const [isInitialized, setIsInitialized] = useState(false);
  
  // Query for saved filters
  const { data: savedFilters = [], isLoading: isSavedFiltersLoading } = useQuery({
    queryKey: ['savedFilters'],
    queryFn: () => SavedFiltersService.readSavedFilters({}),
    staleTime: 1000 * 60 * 5, // 5 minutes
  });
  
  // Query for public filters
  const { data: publicFilters = [], isLoading: isPublicFiltersLoading } = useQuery({
    queryKey: ['publicFilters'],
    queryFn: () => SavedFiltersService.readPublicSavedFilters({}),
    staleTime: 1000 * 60 * 5, // 5 minutes
  });
  
  // Overall loading state
  const isLoading = isSavedFiltersLoading || isPublicFiltersLoading;
  
  // Create a new filter
  const createFilterMutation = useMutation({
    mutationFn: (data: SavedFilterCreate) => SavedFiltersService.createSavedFilter({ requestBody: data }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['savedFilters'] });
      toast(
        'Filter saved',
        'Your filter has been saved successfully.',
        'success'
      );
    },
    onError: (error) => {
      toast(
        'Failed to save filter',
        error.message || 'There was an error saving your filter',
        'error'
      );
    },
  });
  
  // Delete a filter
  const deleteFilterMutation = useMutation({
    mutationFn: (id: number) => SavedFiltersService.deleteSavedFilter({ filterId: id }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['savedFilters'] });
      toast(
        'Filter deleted',
        'Your filter has been deleted successfully.',
        'success'
      );
    },
    onError: (error) => {
      toast(
        'Failed to delete filter',
        error.message || 'There was an error deleting your filter',
        'error'
      );
    },
  });
  
  // Load a filter by name
  const loadFilterByName = async (name: string): Promise<FilterData_Input | null> => {
    try {
      const filter = await SavedFiltersService.readSavedFilterByName({ filterName: name });
      setCurrentFilter(filter.filter_data);
      return filter.filter_data;
    } catch {
      toast(
        'Failed to load filter',
        `Could not find filter with name: ${name}`,
        'error'
      );
      return null;
    }
  };
  
  const saveCurrentFilter = (
    { name, description }: { name: string; description: string | null }
  ) => {
    if (!currentFilter) return;
    createFilterMutation.mutate({ name, description, filter_data: currentFilter });
  };
  
  const initializeDefaultFilter = () => {
    if (isInitialized || isLoading) return;
    
    if (currentFilter) {
      setIsInitialized(true);
      return;
    }
    
    
    const defaultFilter = savedFilters.find((filter) => filter.filter_data.is_default);
    
    if (defaultFilter) {
      setCurrentFilter(defaultFilter.filter_data);
      setIsInitialized(true);
      return;
    }
    
    // If no default filter found in saved filters, check public filters
    const defaultPublicFilter = publicFilters.find((filter) => filter.filter_data.is_default);
    
    if (defaultPublicFilter) {
      setCurrentFilter(defaultPublicFilter.filter_data);
      setIsInitialized(true);
      return;
    }
    
    // If no default filter found at all, create a basic default filter
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
    setIsInitialized(true);
  };
  
  // Update an existing filter
  const updateFilter = (
    id: number,
    { name, description }: { name: string; description: string | null }
  ) => {
    if (!currentFilter) return;
    
    const updateMutation = useMutation({
      mutationFn: () => SavedFiltersService.updateSavedFilter({
        filterId: id,
        requestBody: { name, description, filter_data: currentFilter }
      }),
      onSuccess: () => {
        queryClient.invalidateQueries({ queryKey: ['savedFilters'] });
        toast(
          'Filter updated',
          'Your filter has been updated successfully.',
          'success'
        );
      },
      onError: (error) => {
        toast(
          'Failed to update filter',
          error.message || 'There was an error updating your filter',
          'error'
        );
      },
    });
    
    updateMutation.mutate();
  };
  
  // Auto-initialize the default filter when data is loaded
  useEffect(() => {
    if (!isLoading && !isInitialized) {
      initializeDefaultFilter();
    }
  }, [isLoading, savedFilters, publicFilters]);
  
  // Create the context value
  const contextValue: FilterContextType = {
    // Data
    savedFilters,
    publicFilters,
    currentFilter,
    
    // Status
    isLoading,
    isInitialized,
    
    // Actions
    setCurrentFilter,
    initializeDefaultFilter,
    saveCurrentFilter,
    updateFilter,
    deleteFilter: deleteFilterMutation.mutate,
    loadFilterByName,
  };
  
  return (
    <FilterContext.Provider value={contextValue}>
      {children}
    </FilterContext.Provider>
  );
}

// Custom hook to use the filter context
export function useFilters() {
  const context = useContext(FilterContext);
  
  if (context === undefined) {
    throw new Error('useFilters must be used within a FilterProvider');
  }
  
  return context;
}
