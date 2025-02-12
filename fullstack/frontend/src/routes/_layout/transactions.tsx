import React, { useState } from "react"
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
  Collapse,
  Tabs,
  TabList,
  TabPanels,
  Tab,
  TabPanel,
  ButtonGroup,
  Button,
} from "@chakra-ui/react"
import { TriangleDownIcon, TriangleUpIcon } from "@chakra-ui/icons"
import { createFileRoute } from "@tanstack/react-router"
import { useQuery } from "@tanstack/react-query"
import { TransactionsService } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

// Assume that your client code provides a type alias for the aggregated response.
import type {
  TransactionsGetAggregatedTransactionsResponse,
} from "../../client"

// Define the group-by options. (This might also be imported from your API types.)
export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
})

function Transactions() {
  // State to track which group rows are expanded.
  // Keys are composite: `${sourceId}-${groupId}`
  const [expandedGroups, setExpandedGroups] = useState<{ [key: string]: boolean }>(
    {}
  )

  // State for the grouping option (default to grouping by category)
  const [groupingOption, setGroupingOption] = useState<GroupByOption>(
    GroupByOption.category
  )

  const toggleGroup = (sourceId: number, groupId: number) => {
    const key = `${sourceId}-${groupId}`
    setExpandedGroups((prev) => ({
      ...prev,
      [key]: !prev[key],
    }))
  }

  const { data, isLoading, error } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", groupingOption],
    queryFn: () =>
      TransactionsService.getAggregatedTransactions({ groupBy: groupingOption }),
    enabled: isLoggedIn(),
  })

  console.log(data)

  return (
    <Container maxW="full" py={8}>
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} py={12}>
        Transactions
      </Heading>

      {/* Button group to select the grouping option */}
      <ButtonGroup mb={6} isAttached variant="outline">
        <Button
          onClick={() => setGroupingOption(GroupByOption.category)}
          colorScheme={
            groupingOption === GroupByOption.category ? "blue" : "gray"
          }
        >
          Category
        </Button>
        <Button
          onClick={() => setGroupingOption(GroupByOption.month)}
          colorScheme={groupingOption === GroupByOption.month ? "blue" : "gray"}
        >
          Month
        </Button>
        <Button
          onClick={() => setGroupingOption(GroupByOption.year)}
          colorScheme={groupingOption === GroupByOption.year ? "blue" : "gray"}
        >
          Year
        </Button>
      </ButtonGroup>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading transactions.</Text>
      ) : data && data.groups && data.groups.length > 0 ? (
        <Tabs variant="enclosed">
          <TabList>
            {data.groups.map((sourceGroup) => (
              <Tab key={sourceGroup.transaction_source_id}>
                {sourceGroup.transaction_source_name}
              </Tab>
            ))}
          </TabList>
          <TabPanels>
            {data.groups.map((sourceGroup) => (
              <TabPanel key={sourceGroup.transaction_source_id}>
                <TableContainer>
                  <Table variant="simple">
                    <Thead>
                      <Tr>
                        <Th></Th>
                        <Th>Group</Th>
                        <Th isNumeric>Withdrawals</Th>
                        <Th isNumeric>Deposits</Th>
                        <Th isNumeric>Balance</Th>
                      </Tr>
                    </Thead>
                    <Tbody>
                      {sourceGroup.groups.map((group) => (
                        <React.Fragment key={group.category_id}>
                          {/* Aggregated row */}
                          <Tr
                            onClick={() =>
                              toggleGroup(
                                sourceGroup.transaction_source_id,
                                group.category_id
                              )
                            }
                            style={{ cursor: "pointer" }}
                          >
                            <Td>
                              {expandedGroups[
                                `${sourceGroup.transaction_source_id}-${group.category_id}`
                              ] ? (
                                <TriangleDownIcon />
                              ) : (
                                <TriangleUpIcon />
                              )}
                            </Td>
                            <Td>{group.category_name}</Td>
                            <Td isNumeric>{group.total_withdrawals}</Td>
                            <Td isNumeric>{group.total_deposits}</Td>
                            <Td isNumeric>{group.total_balance}</Td>
                          </Tr>
                          {/* Detail row: nested table of individual transactions */}
                          <Tr>
                            <Td colSpan={5} p={0}>
                              <Collapse
                                in={
                                  expandedGroups[
                                    `${sourceGroup.transaction_source_id}-${group.category_id}`
                                  ]
                                }
                                animateOpacity
                              >
                                <Box p={4}>
                                  <Table variant="simple" size="sm">
                                    <Thead>
                                      <Tr>
                                        <Th>ID</Th>
                                        <Th>Description</Th>
                                        <Th>Date</Th>
                                        <Th isNumeric>Amount</Th>
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
                                          <Td isNumeric>{transaction.amount}</Td>
                                          <Td>{transaction.kind}</Td>
                                          <Td>
                                            {transaction.archived ? "Yes" : "No"}
                                          </Td>
                                        </Tr>
                                      ))}
                                    </Tbody>
                                  </Table>
                                </Box>
                              </Collapse>
                            </Td>
                          </Tr>
                        </React.Fragment>
                      ))}
                      {/* Overall totals for this transaction source */}
                      <Tr fontWeight="bold">
                        <Td colSpan={2}>Source Totals</Td>
                        <Td isNumeric>{sourceGroup.total_withdrawals}</Td>
                        <Td isNumeric>{sourceGroup.total_deposits}</Td>
                        <Td isNumeric>{sourceGroup.total_balance}</Td>
                      </Tr>
                    </Tbody>
                  </Table>
                </TableContainer>
              </TabPanel>
            ))}
          </TabPanels>
        </Tabs>
      ) : (
        <Text>No transactions found.</Text>
      )}
    </Container>
  )
}

export default Transactions
