import { BudgetsService } from "@/client"
import { ManageBudget } from "@/components/Common/BudgetManager"
import { isLoggedIn } from "@/hooks/useAuth"
import {
  Box,
  Button,
  Container,
  Flex,
  HStack,
  Heading,
  Input,
  Text,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { useState } from "react"

export const Route = createFileRoute("/_layout/budget")({
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
    enabled: isLoggedIn(),
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

  if (isLoading) {
    return (
      <Container maxW="full">
        <Heading size="lg" textAlign="center" py={12}>
          Loading...
        </Heading>
      </Container>
    )
  }

  return (
    <Container mt={24} maxW="large">
      <Text fontSize="lg" textAlign="center" py={12}>
        the budget feature is still very much in development, so consider it
        lucky when things work :)
      </Text>
      {budget ? (
        <ManageBudget budget={budget} />
      ) : (
        <Flex justifyContent="center">
          <CreateNewBudget />
        </Flex>
      )}
    </Container>
  )
}

function CreateNewBudget() {
  const queryClient = useQueryClient()
  const [budgetName, setBudgetName] = useState<string>("My Budget")

  const addBudgetMutation = useMutation({
    mutationFn: () =>
      BudgetsService.createBudget({
        requestBody: { name: budgetName },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] })
      setBudgetName("")
    },
  })

  return (
    <Box>
      <Text>You don't have a budget yet.</Text>
      <HStack mt={4}>
        <Input
          maxWidth="md"
          value={budgetName}
          onChange={(e) => setBudgetName(e.target.value)}
        />
        <Button size="sm" onClick={() => addBudgetMutation.mutate()}>
          Create Budget
        </Button>
      </HStack>
    </Box>
  )
}
