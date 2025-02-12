import { Box, Container, Text } from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"

import useAuth, { isLoggedIn } from "../../hooks/useAuth"
import { useQuery, useQueryClient } from "@tanstack/react-query"
import { TransactionOut, TransactionsService } from "../../client"

export const Route = createFileRoute("/_layout/")({
  component: Dashboard,
})

function Dashboard() {
  const { user: currentUser } = useAuth()


  const { data, isLoading } = useQuery<TransactionOut | null, Error>({
    queryKey: ["transactions"],
    queryFn: TransactionsService.getTransactions,
    enabled: isLoggedIn(),
  })

  console.log(data)


  return (
    <>
      <Container maxW="full">
        <Box pt={12} m={4}>
          <Text fontSize="2xl">
            Hi, {currentUser?.full_name || currentUser?.email} 👋🏼
          </Text>
          <Text>Welcome back, nice to see you again!</Text>
        </Box>
      </Container>
    </>
  )
}
