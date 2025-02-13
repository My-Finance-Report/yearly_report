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
import {BarChartLocal} from "@/components/Charting/BarChart"
import { TriangleDownIcon, TriangleUpIcon } from "@chakra-ui/icons"
import { createFileRoute } from "@tanstack/react-router"
import { useQuery } from "@tanstack/react-query"
import { TransactionsService } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

import type {
  TransactionsGetAggregatedTransactionsResponse,
  AggregatedGroup,
} from "../../client"

// Define the group-by options.
export enum GroupByOption {
  category = "category",
  month = "month",
  year = "year",
}

const availableOptions: GroupByOption[] = [
  GroupByOption.category,
  GroupByOption.year,
  GroupByOption.month,
]

export const Route = createFileRoute("/_layout/transactions")({
  component: Transactions,
})

function Transactions() {
  // State for expanded rows; keys are composite: `${sourceId}-${groupId}-${path}`
  const [expandedGroups, setExpandedGroups] = useState<{ [key: string]: boolean }>(
    {}
  )

  // State for the grouping options; default is just category.
  const [groupingOptions, setGroupingOptions] = useState<GroupByOption[]>([
    GroupByOption.category,
  ])

  // Toggle a grouping option on/off.
  const toggleGroupingOption = (option: GroupByOption) => {
    setGroupingOptions((prev) => {
      // Ensure at least one option remains selected.
      if (prev.includes(option)) {
        if (prev.length === 1) return prev
        return prev.filter((o) => o !== option)
      } else {
        // Add the option. The final order will follow availableOptions.
        const newOptions = [...prev, option]
        return availableOptions.filter((o) => newOptions.includes(o))
      }
    })
  }

  // Toggle expand for a given composite key.
  const toggleGroup = (sourceId: number, groupKey: string) => {
    const key = `${sourceId}-${groupKey}`
    setExpandedGroups((prev) => ({
      ...prev,
      [key]: !prev[key],
    }))
  }

  // Query the aggregated endpoint.
  const { data, isLoading, error } = useQuery<
    TransactionsGetAggregatedTransactionsResponse,
    Error
  >({
    queryKey: ["aggregatedTransactions", groupingOptions],
    queryFn: () =>
      // Pass the array of grouping options to your service.
      TransactionsService.getAggregatedTransactions({ groupBy: groupingOptions }),
    enabled: isLoggedIn(),
  })


  // Recursive function to render nested groups.
  const renderGroups = (
    groups: AggregatedGroup[],
    sourceId: number,
    pathPrefix: string = ""
  ) => {
    return groups.map((group, idx) => {
      // Create a composite key for this group.
      // We'll use the group's own id (assumed to be group.group_id) for toggling.
      const groupKey = pathPrefix ? `${pathPrefix}-${group.group_id}` : `${group.group_id}`
      const isExpanded = expandedGroups[`${sourceId}-${groupKey}`] || false

      return (
        <React.Fragment key={groupKey}>
          {/* Aggregated row for this group */}
          <Tr
            onClick={() => toggleGroup(sourceId, groupKey)}
            style={{ cursor: "pointer" }}
          >
            <Td pl={pathPrefix ? Number(pathPrefix.split("-").length) * 4 : 0}>
              {isExpanded ? <TriangleDownIcon /> : <TriangleUpIcon />}
            </Td>
            <Td>{group.group_name}</Td>
            <Td isNumeric>{group.total_withdrawals}</Td>
            <Td isNumeric>{group.total_deposits}</Td>
            <Td isNumeric>{group.total_balance}</Td>
          </Tr>
          <Tr>
            <Td colSpan={5} p={0}>
              <Collapse in={isExpanded} animateOpacity>
                <Box pl={4}>
                  {group.subgroups && group.subgroups.length > 0 ? (
                    // Recursively render subgroups.
                    <Table variant="simple" size="sm">
                      <Thead>
                        <Tr>
                          <Th></Th>
                          <Th>Group</Th>
                          <Th isNumeric>Withdrawals</Th>
                          <Th isNumeric>Deposits</Th>
                          <Th isNumeric>Balance</Th>
                        </Tr>
                      </Thead>
                      <Tbody>{renderGroups(group.subgroups, sourceId, groupKey)}</Tbody>
                    </Table>
                  ) : (
                    // Leaf group: render the transactions.
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
                        {group.transactions?.map((transaction) => (
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
                            <Td>{transaction.archived ? "Yes" : "No"}</Td>
                          </Tr>
                        ))}
                      </Tbody>
                    </Table>
                  )}
                </Box>
              </Collapse>
            </Td>
          </Tr>
        </React.Fragment>
      )
    })
  }

  return (
    <Container maxW="full" py={8}>
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} py={12}>
        Transactions
      </Heading>


      <ButtonGroup mb={6} isAttached variant="outline">
        {availableOptions.map((option) => (
          <Button
            key={option}
            onClick={() => toggleGroupingOption(option)}
            colorScheme={groupingOptions.includes(option) ? "blue" : "gray"}
          >
            {option.charAt(0).toUpperCase() + option.slice(1)}
          </Button>
        ))}
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
                      {renderGroups(
                        sourceGroup.groups,
                        sourceGroup.transaction_source_id
                      )}
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
