import { BudgetsService } from "@/client"
import { ManageBudget } from "@/components/Common/BudgetManager"
import { isSessionActive } from "@/hooks/useAuth"
import {
  Container,
  Flex,
  Heading,
  Spinner,
} from "@chakra-ui/react"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/_logged_in/budget")({
  component: ManageBudgets,
})

function ManageBudgets() {
  const {
    data: budget,
    isLoading,
    isError,
  } = useQuery({
    queryKey: ["budgets"],
    queryFn: BudgetsService.getBudget,
    enabled: isSessionActive(),
  })

  const {
      data: budgetStatus,
      isLoading: statusLoading,
      isError: statusIsError,
    } = useQuery({
      queryKey: ["budgetStatus"],
      queryFn: BudgetsService.getBudgetStatus,
      enabled: isSessionActive(),
    })


  if (isError||statusIsError) {
    return (
      <Container maxW="full">
        <Heading size="lg" textAlign="center" py={12}>
          Failed to Load Budgets
        </Heading>
      </Container>
    )
  }

  if (isLoading || statusLoading) {
    return (
      <Container maxW="full">
        <Heading size="lg" textAlign="center" py={12}>
          <Spinner />
        </Heading>
      </Container>
    )
  }

  return (
    <Flex alignItems="center" justifyContent="center">
        <ManageBudget budget={budget} budgetStatus={budgetStatus} />
    </Flex>
  )
}


