import { useCallback, useEffect, useState } from "react"

/**
 * A hook that synchronizes filter state with URL search parameters
 *
 * @param defaultValues - The default values for filters if not present in URL
 * @returns An object with filter values and setter functions
 */
export function useFilterState(
  defaultValues: {
    years?: string[]
    accounts?: string[]
    months?: string[]
    categories?: string[]
    budgets?: string[]
  } = {},
) {
  // Initialize state from URL or defaults
  const [years, setYearsState] = useState<string[]>(
    () => getParamAsArray("years") || defaultValues.years || [],
  )
  const [accounts, setAccountsState] = useState<string[]>(
    () => getParamAsArray("accounts") || defaultValues.accounts || [],
  )
  const [months, setMonthsState] = useState<string[]>(
    () => getParamAsArray("months") || defaultValues.months || [],
  )
  const [categories, setCategoriesState] = useState<string[]>(
    () => getParamAsArray("categories") || defaultValues.categories || [],
  )
  const [budgets, setBudgetsState] = useState<string[]>(
    () => getParamAsArray("budgets") || defaultValues.budgets || [],
  )

  // Helper to get URL parameters as arrays
  function getParamAsArray(param: string): string[] | undefined {
    const value = new URLSearchParams(window.location.search).get(param)
    return value ? value.split(",").filter(Boolean) : undefined
  }

  // Update URL when any filter changes
  useEffect(() => {
    const searchParams = new URLSearchParams(window.location.search)

    updateSearchParam(searchParams, "years", years)
    updateSearchParam(searchParams, "accounts", accounts)
    updateSearchParam(searchParams, "months", months)
    updateSearchParam(searchParams, "categories", categories)
    updateSearchParam(searchParams, "budgets", budgets)

    const newSearch = searchParams.toString()
    const currentPath = window.location.pathname

    // Only update if search params have changed
    if (newSearch !== window.location.search.replace(/^\?/, "")) {
      const newUrl = `${currentPath}${newSearch ? `?${newSearch}` : ""}`
      window.history.replaceState(null, "", newUrl)
    }
  }, [years, accounts, months, categories, budgets])

  // Helper to update search parameters
  function updateSearchParam(
    params: URLSearchParams,
    key: string,
    values: string[],
  ) {
    if (values && values.length > 0) {
      params.set(key, values.join(","))
    } else {
      params.delete(key)
    }
  }

  // Create wrapper setters that update both state and URL
  const setYears = useCallback((value: React.SetStateAction<string[]>) => {
    setYearsState(value)
  }, [])

  const setAccounts = useCallback((value: React.SetStateAction<string[]>) => {
    setAccountsState(value)
  }, [])

  const setMonths = useCallback((value: React.SetStateAction<string[]>) => {
    setMonthsState(value)
  }, [])

  const setCategories = useCallback((value: React.SetStateAction<string[]>) => {
    setCategoriesState(value)
  }, [])

  const setBudgets = useCallback((value: React.SetStateAction<string[]>) => {
    setBudgetsState(value)
  }, [])

  return {
    years,
    accounts,
    months,
    categories,
    budgets,
    setYears,
    setAccounts,
    setMonths,
    setCategories,
    setBudgets,
  }
}
