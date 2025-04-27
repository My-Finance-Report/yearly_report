import { AccountsService } from "@/client";
import { AccountDetails } from "@/components/Common/AccountDetails";
import { OnboardDialogs } from "@/components/Common/OnboardModal/Onboarding";
import {
  Badge,
  Box,
  Button,
  Container,
  Flex,
  Icon,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  Spinner,
  Text,
  createListCollection,
  useDisclosure,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { useState } from "react";
import { FaPlus } from "react-icons/fa";

export const Route = createFileRoute("/_layout/_logged_in/manage-accounts")({
  component: ManageAccounts,
});

function ManageAccounts() {
  const {
    data: accounts,
    isLoading: isLoadingAccounts,
    isError,
  } = useQuery({
    queryKey: ["accounts"],
    queryFn: AccountsService.getTransactionSources,
  });

  const {
    open: isAddAccountOpen,
    onOpen: onAddAccountOpen,
    onClose: onAddAccountClose,
  } = useDisclosure();

  const [selectedAccountIndex, setSelectedAccountIndex] = useState<number>(0);

  const handleAccountChange = (details: { value: string[] }) => {
    if (details.value.length > 0) {
      setSelectedAccountIndex(Number.parseInt(details.value[0], 10));
    }
  };

  const isPlaidLinked = (accountId: number) => {
    const account = accounts?.find((a) => a.id === accountId);
    return account?.is_plaid_connected || false;
  };

  if (isError) {
    return (
      <Container maxW="full">
        <Text>Failed to load accounts. Please try again.</Text>
      </Container>
    );
  }

  return (
    <Container mt={8} maxW="container.xl" minH="75vh">
      <Flex justifyContent="space-between" alignItems="center" mb={6}>
        <Button onClick={onAddAccountOpen}>
          <Flex align="center">
            <Icon as={FaPlus} mr={2} />
            <Text>Add Account</Text>
          </Flex>
        </Button>
      </Flex>

      {isLoadingAccounts ? (
        <Flex justify="center" align="center" height="300px">
          <Spinner size="xl" />
        </Flex>
      ) : accounts && accounts.length > 0 ? (
        <Box>
          <Box mb={6}>
            <SelectRoot
              id="account_selector"
              size="lg"
              collection={createListCollection({
                items: accounts.map((account, index) => ({
                  label: account.name,
                  value: index.toString(),
                })),
              })}
              value={[selectedAccountIndex.toString()]}
              onValueChange={handleAccountChange}
            >
              <SelectTrigger>
                <Flex align="center" width="100%">
                  <SelectValueText placeholder="Select an account" />
                  {accounts[selectedAccountIndex]?.archived && (
                    <Badge ml={2} colorScheme="red" variant="solid" size="sm">
                      Archived
                    </Badge>
                  )}
                </Flex>
              </SelectTrigger>
              <SelectContent>
                {accounts.map((account, index) => (
                  <SelectItem
                    key={account.id}
                    item={{
                      label: account.name,
                      value: index.toString(),
                    }}
                  >
                    <Flex align="center">
                      {account.name}
                      {account.archived && (
                        <Badge
                          ml={2}
                          colorScheme="red"
                          variant="solid"
                          size="sm"
                        >
                          Archived
                        </Badge>
                      )}
                    </Flex>
                  </SelectItem>
                ))}
              </SelectContent>
            </SelectRoot>
          </Box>

          {accounts[selectedAccountIndex] && (
            <AccountDetails
              accountId={accounts[selectedAccountIndex].id}
              accountName={accounts[selectedAccountIndex].name}
              accountType={"depository"}
              isPlaidLinked={isPlaidLinked(accounts[selectedAccountIndex].id)}
              isArchived={accounts[selectedAccountIndex].archived}
            />
          )}
        </Box>
      ) : (
        <Box textAlign="center" p={10} borderWidth="1px" borderRadius="lg">
          <Text fontSize="lg">You don't have any accounts yet.</Text>
          <Text>Create a new account or link your bank to get started.</Text>
        </Box>
      )}

      <OnboardDialogs
        isOnboardOpen={isAddAccountOpen}
        onOnboardClose={onAddAccountClose}
        isDialog={true}
      />
    </Container>
  );
}
