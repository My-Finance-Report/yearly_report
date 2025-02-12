import {
  Container,
  Heading,
  Box,
  Table,
  TableContainer,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Spinner,
  Text,
} from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"
import { useQuery } from "@tanstack/react-query"
import { TransactionsService } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

// Assume that your client code provides a type alias for the aggregated response.
// For example, it might be named TransactionsGetAggregatedTransactionsResponse.
import type {
  TransactionsGetAggregatedTransactionsResponse,
} from "../../client"

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
})

function Transactions() {
  // Use the aggregated endpoint.
  const { data, isLoading, error } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions"],
    queryFn: TransactionsService.getAggregatedTransactions,
    enabled: isLoggedIn(),
  })

  console.log(data)

  return (
    <Container maxW="full" py={8}>
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} py={12}>
        Transactions
      </Heading>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data && data.groups && data.groups.length > 0 ? (
        <>
          {data.groups.map((group) => (
            <Box key={group.category_id} mb={8}>
              <Heading size="md" mb={2}>
                Category: {group.category_id}
              </Heading>
              <Text>
                Withdrawals: {group.total_withdrawals} | Deposits:{" "}
                {group.total_deposits} | Balance: {group.total_balance}
              </Text>

              <TableContainer mt={4}>
                <Table variant="simple">
                  <Thead>
                    <Tr>
                      <Th>ID</Th>
                      <Th>Description</Th>
                      <Th>Date of Transaction</Th>
                      <Th>Amount</Th>
                      <Th>Kind</Th>
                      <Th>Archived</Th>
                    </Tr>
                  </Thead>
                  <Tbody>
                    {group.transactions.map((transaction) => (
                      <Tr key={transaction.id}>
                        <Td>{transaction.id}</Td>
                        <Td>{transaction.description}</Td>
                        <Td>
                          {new Date(
                            transaction.date_of_transaction
                          ).toLocaleDateString()}
                        </Td>
                        <Td>{transaction.amount}</Td>
                        <Td>{transaction.kind}</Td>
                        <Td>{transaction.archived ? "Yes" : "No"}</Td>
                      </Tr>
                    ))}
                  </Tbody>
                </Table>
              </TableContainer>
            </Box>
          ))}

          <Box mt={8} p={4} borderWidth={1} borderRadius="md">
            <Heading size="md" mb={2}>
              Overall Totals
            </Heading>
            <Text>
              Withdrawals: {data.overall_withdrawals} | Deposits:{" "}
              {data.overall_deposits} | Balance: {data.overall_balance}
            </Text>
          </Box>
        </>
      ) : (
        <Text>No transactions found.</Text>
      )}
    </Container>
  )
}

export default Transactions
