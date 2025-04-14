import { BudgetsService } from "@/client"
import { ManageBudget } from "@/components/Common/BudgetManager"
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
      data,
      isLoading: statusLoading,
      isError
    } = useQuery({
      queryKey: ["budgetStatus"],
      queryFn: BudgetsService.getBudgetStatus,
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

  if (statusLoading) {
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
        <ManageBudget budgetStatus={data!} />
    </Flex>
  )
}


