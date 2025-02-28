import { AccountsService } from "@/client"
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

export const Route = createFileRoute("/_layout/manage-accounts")({
  component: ManageAccounts,
})

function ManageAccounts() {
  const {
    data: accounts,
    isLoading,
    isError,
  } = useQuery({
    queryKey: ["accounts"],
    queryFn: AccountsService.getTransactionSources,
    enabled: isLoggedIn(),
  })

  const tabs = useTabs({
    defaultValue: "0",
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
    <Container mt={24} maxW="large">
      {isLoading ? (
        <Spinner size="lg" />
      ) : (
        <Tabs.RootProvider variant="enclosed" value={tabs}>
          <Flex justifyContent="center">
            <Tabs.List>
              {accounts?.map((account, index) => (
                <Tabs.Trigger key={account.id} value={index.toString()}>
                  {account.name}
                </Tabs.Trigger>
              ))}
            </Tabs.List>
          </Flex>

          {accounts?.map((account, index) => (
            <Tabs.Content key={account.id} value={index.toString()}>
              <VStack spaceX={6}>
                <CategoriesManager accountId={account.id} />
              </VStack>
            </Tabs.Content>
          ))}
        </Tabs.RootProvider>
      )}
    </Container>
  )
}
