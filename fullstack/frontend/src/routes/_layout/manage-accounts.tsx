import { AccountsService } from "@/client"
import { isLoggedIn } from "@/hooks/useAuth"
import { CategoriesManager} from "@/components/Common/CategoriesManager"
import {
  Container,
  Heading,
  Tabs,
  VStack,
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
        <Tabs.Root variant="enclosed">
          <Tabs.List>
            {accounts?.map((account) => (
              <Tabs.Trigger value={account.id.toString()}>{account.name}</Tabs.Trigger>
            ))}
          </Tabs.List>

            {accounts?.map((account) => (
              <Tabs.Content value={account.id.toString()}>
                <VStack spacing={6} align="start">
                  <CategoriesManager accountId={account.id} />
                </VStack>
              </Tabs.Content>
            ))}
        </Tabs.Root>
      )}
    </Container>
  )
}
