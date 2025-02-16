import { AccountsService } from "@/client"
import { isLoggedIn } from "@/hooks/useAuth"
import { CategoriesManager} from "@/components/Common/CategoriesManager"
import {
  Container,
  Heading,
  Tab,
  TabList,
  TabPanel,
  TabPanels,
  Tabs,
  Button,
  VStack,
  Box,
  Spinner,
} from "@chakra-ui/react"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"


export const Route = createFileRoute("/_layout/manage-accounts")({
  component: ManageAccounts,
})

function ManageAccounts() {

  const { data: accounts, isLoading, isError } = useQuery({
    queryKey: ["accounts"],
    queryFn: AccountsService.getTransactionSources,
    enabled: isLoggedIn(),
  })

  if (isError) {
    return (
      <Container maxW="full">
        <Heading size="lg" textAlign="center" py={12}>
          Failed to Load Accounts
        </Heading>
      </Container>
    )
  }

  return (
    <Container maxW="full">
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} py={12}>
        Manage Accounts
      </Heading>

      {isLoading ? (
        <Spinner size="lg" />
      ) : (
        <Tabs variant="enclosed">
          <TabList>
            {accounts?.map((account) => (
              <Tab key={account.id}>{account.name}</Tab>
            ))}
          </TabList>

          <TabPanels>
            {accounts?.map((account) => (
              <TabPanel key={account.id}>
                <VStack spacing={6} align="start">
                  <CategoriesManager accountId={account.id} />

                  <Box>
                    <Button colorScheme="blue" size="sm" mr={2}>
                      Add New Account
                    </Button>
                    <Button colorScheme="yellow" size="sm" mr={2}>
                      Edit Account
                    </Button>
                    <Button colorScheme="red" size="sm">
                      Remove Account
                    </Button>
                  </Box>
                </VStack>
              </TabPanel>
            ))}
          </TabPanels>
        </Tabs>
      )}
    </Container>
  )
}
