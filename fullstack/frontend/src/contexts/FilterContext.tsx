import { Route } from "@/routes/_layout/_logged_in/transactions"
import {
  type SavedFilterOut,
  type SavedFilterCreate,
  SavedFiltersService,
} from "@/client"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import React, {
  type ReactNode,
  createContext,
  useContext,
  useEffect,
  useState,
} from "react"
import useCustomToast from "../hooks/useCustomToast"
import { useNavigate } from "@tanstack/react-router"

// Define the context type
interface FilterContextType {
  // Data
  savedFilters: SavedFilterOut[]
  currentFilter: SavedFilterOut 

  // Status
  isLoading: boolean

  // Actions
  setCurrentFilter: React.Dispatch<React.SetStateAction<SavedFilterOut>>
  initializeDefaultFilter: () => void
  saveCurrentFilter: (data: {
    name: string
    description: string | null
  }) => void
  updateFilter: (
    id: number,
    data: { name: string; description: string | null },
  ) => void
  deleteFilter: (id: number) => void
  loadFilterByName: (name: string) => Promise<SavedFilterOut | null>
}


const FilterContext = createContext<FilterContextType | undefined>(undefined)

export function FilterProvider({
  children,
  isDemo,
}: { children: ReactNode; isDemo?: boolean }) {
  const queryClient = useQueryClient()
  const toast = useCustomToast()

  const { data: savedFilters = [], isLoading } = useQuery({
    queryKey: ["savedFilters"],
    queryFn: () => SavedFiltersService.readSavedFilters({}),
    enabled: !isDemo,
  })

  const [currentFilter, setCurrentFilter] = useState<SavedFilterOut>(savedFilters[0])

  const navigate = useNavigate()

  useEffect(() => {
    if (currentFilter) {
      navigate({
        search: (prev: Record<string, unknown>) => ({
          ...prev,
          filter: currentFilter.name,
        }),
        replace: true,
    });

    }
  }, [currentFilter])

  const createFilterMutation = useMutation({
    mutationFn: (data: SavedFilterCreate) =>
      SavedFiltersService.createSavedFilter({ requestBody: data }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["savedFilters"] })
      toast(
        "Filter saved",
        "Your filter has been saved successfully.",
        "success",
      )
    },
    onError: () => {
      toast(
        "Failed to save filter",
        isDemo
          ? "you can't save filters in demo mode :("
          : "There was an error saving your filter",
        "error",
      )
    },
  })

  const deleteFilterMutation = useMutation({
    mutationFn: (id: number) =>
      SavedFiltersService.deleteSavedFilter({ filterId: id }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["savedFilters"] })
      initializeDefaultFilter()
      toast(
        "Filter deleted",
        "Your filter has been deleted successfully.",
        "success",
      )
    },
    onError: (error) => {
      toast(
        "Failed to delete filter",
        error.message || "There was an error deleting your filter",
        "error",
      )
    },
  })

  const loadFilterByName = async (
    name: string,
  ): Promise<SavedFilterOut | null> => {
    const filter = savedFilters.find((f) => f.name === name)
    if (filter) {
      setCurrentFilter(filter)
      return filter
    }
    if (name === "custom") {
      return null
    }
    try {
      const filter = await SavedFiltersService.readSavedFilterByName({
        filterName: name,
      })
      setCurrentFilter(filter)
      return filter
    } catch {
      navigate({
        search: (prev: Record<string, unknown>) => ({
          ...prev,
          filter: savedFilters[0]?.name,
        }),
        replace: true,
      })

      return null
    }
  }

  const saveCurrentFilter = ({
    name,
    description,
  }: { name: string; description: string | null }) => {
    if (!currentFilter) return
    createFilterMutation.mutate({
      name,
      description,
      filter_data: currentFilter.filter_data,
    })
  }

  const { filter : filterFromUrl } = Route.useSearch()

  function getInitalFilter(): SavedFilterOut {
    if (filterFromUrl) {
      loadFilterByName(filterFromUrl).then((f) => {
        if (f){
          return f
        }
      })
    }

    if (currentFilter) {
      return currentFilter
    }

    const defaultFilter = savedFilters.find(
      (filter) => filter.filter_data.is_default,
    )

    if (defaultFilter) {
      return defaultFilter
    }

    return savedFilters[0]
  }


  const initializeDefaultFilter = () => {
    setCurrentFilter(getInitalFilter())
  }

  const updateFilter = (
    id: number,
    { name, description }: { name: string; description: string | null },
  ) => {
    if (!currentFilter) return

    const updateMutation = useMutation({
      mutationFn: () =>
        SavedFiltersService.updateSavedFilter({
          filterId: id,
          requestBody: { name, description, filter_data: currentFilter.filter_data },
        }),
      onSuccess: () => {
        queryClient.invalidateQueries({ queryKey: ["savedFilters"] })
        toast(
          "Filter updated",
          "Your filter has been updated successfully.",
          "success",
        )
      },
      onError: (error) => {
        toast(
          "Failed to update filter",
          error.message || "There was an error updating your filter",
          "error",
        )
      },
    })

    updateMutation.mutate()
  }

  const contextValue: FilterContextType = {
    savedFilters,
    currentFilter: currentFilter,
    isLoading,
    setCurrentFilter,
    initializeDefaultFilter,
    saveCurrentFilter,
    updateFilter,
    deleteFilter: deleteFilterMutation.mutate,
    loadFilterByName,
  }

  return (
    <FilterContext.Provider value={contextValue}>
      {children}
    </FilterContext.Provider>
  )
}

export function useFilters() {
  const context = useContext(FilterContext)

  if (context === undefined) {
    throw new Error("useFilters must be used within a FilterProvider")
  }

  return context
}
