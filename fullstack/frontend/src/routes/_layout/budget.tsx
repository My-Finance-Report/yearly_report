import { BudgetService } from "@/client"
import { CategoriesManager } from "@/components/Common/CategoriesManager"
import { isLoggedIn } from "@/hooks/useAuth"
import {
  Container,
  Flex,
  Heading,
  Spinner,
  Tabs,
  VStack,
  useTabs,
} from "@chakra-ui/react"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/budget")({
  component: ManageBudgets,
})

function ManageBudgets() {
  const {
    data: budgets,
    isLoading,
    isError,
  } = useQuery({
    queryKey: ["budgets"],
    queryFn: BudgetService.getBudget,
    enabled: isLoggedIn(),
  })

  const tabs = useTabs({
    defaultValue: "0",
  })

  if (isError) {
    return (
      <Container maxW="full">
        <Heading size="lg" textAlign="center" py={12}>
          Failed to Load Budgets
        </Heading>
      </Container>
    )
  }

  return (
    <Container mt={24} maxW="large">
      {isLoading ? (
        <Spinner size="lg" />
      ) : (
        <Tabs.RootProvider variant="enclosed" value={tabs}>
          <Flex justifyContent="center">
            <Tabs.List>
              {budgets?.map((budget, index) => (
                <Tabs.Trigger key={budget.id} value={index.toString()}>
                  {budget.name}
                </Tabs.Trigger>
              ))}
            </Tabs.List>
          </Flex>

          {budgets?.map((budget, index) => (
            <Tabs.Content key={budget.id} value={index.toString()}>
              <VStack spaceX={6}>
                <CategoriesManager accountId={budget.id} />
              </VStack>
            </Tabs.Content>
          ))}
        </Tabs.RootProvider>
      )}
    </Container>
  )
}
