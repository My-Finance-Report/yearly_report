import { type PlaidGetPlaidAccountsResponse, PlaidService } from "@/client";
import {
  Box,
  Button,
  CardBody,
  CardRoot,
  Flex,
  Heading,
  Icon,
  SimpleGrid,
  Spinner,
  Text,
  VStack,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import React, { useState, useCallback, useEffect } from "react";
import {
  FaCreditCard,
  FaMoneyBillWave,
  FaPlus,
  FaUniversity,
} from "react-icons/fa";
import { usePlaidLink } from "react-plaid-link";
import useCustomToast from "../../../hooks/useCustomToast";

export const Route = createFileRoute("/_layout/_logged_in/plaid")({
  component: PlaidIntegration,
});

function PlaidIntegration() {
  const [linkToken, setLinkToken] = useState<string | null>(null);
  const toast = useCustomToast();
  const queryClient = useQueryClient();

  const { data: accounts, isLoading: isLoadingAccounts } =
    useQuery<PlaidGetPlaidAccountsResponse>({
      queryKey: ["plaidAccounts"],
      queryFn: () => PlaidService.getPlaidAccounts(),
    });

  const createLinkTokenMutation = useMutation({
    mutationFn: async () => {
      const response = await PlaidService.getLinkToken();
      return response;
    },
    onSuccess: (data) => {
      setLinkToken(data.link_token);
    },
    onError: () => {
      toast("Error", "Could not create link token", "error");
    },
  });

  // Exchange public token for access token
  const exchangeTokenMutation = useMutation({
    mutationFn: async (public_token: string) => {
      const response = await PlaidService.exchangeToken({
        requestBody: {
          public_token,
        },
      });
      return response;
    },
    onSuccess: () => {
      toast("Success", "Account connected successfully", "success");
      queryClient.invalidateQueries({ queryKey: ["plaidAccounts"] });
    },
    onError: () => {
      toast("Error", "Failed to connect account", "error");
    },
  });

  // Initialize Plaid Link
  const { open, ready } = usePlaidLink({
    token: linkToken,
    onSuccess: (public_token) => {
      exchangeTokenMutation.mutate(public_token);
      setLinkToken(null);
    },
    onExit: (err) => {
      if (err) {
        toast(
          "Connection Error",
          err.display_message || "Error connecting to your bank",
          "error",
        );
      }
    },
  });

  useEffect(() => {
    if (!linkToken) {
      createLinkTokenMutation.mutate();
    }
  }, []);

  useEffect(() => {
    if (!linkToken) {
      createLinkTokenMutation.mutate();
    }
  }, []);

  const handleConnectAccount = useCallback(() => {
    if (ready && linkToken) {
      open();
    } else {
      createLinkTokenMutation.mutate();
    }
  }, [ready, linkToken, open]);

  const getAccountIcon = (type: string) => {
    switch (type) {
      case "credit":
        return FaCreditCard;
      case "investment":
        return FaMoneyBillWave;
      default:
        return FaUniversity;
    }
  };

  const isLoading =
    createLinkTokenMutation.isPending || exchangeTokenMutation.isPending;

  return (
    <Box p={5}>
      <Box mb={6} bg="yellow.800" p={4} borderRadius="lg" boxShadow="sm">
        <Text fontSize="lg" color="white" fontWeight="bold">
          Heads up! It often takes a few minutes for your accounts to sync after
          connecting them.
        </Text>
      </Box>
      <VStack align="" gap={6}>
        <Heading size="lg">Connect Your Accounts</Heading>
        <Text>
          Securely connect your bank accounts, credit cards, and investment
          accounts to automatically import transactions and track your finances.
        </Text>

        <Button
          colorScheme="blue"
          size="lg"
          maxWidth="200px"
          onClick={handleConnectAccount}
          disabled={!ready && !isLoading}
          mb={6}
        >
          <FaPlus />
          Connect an Account
        </Button>

        <Heading size="md" mb={4}>
          Connected Accounts
        </Heading>

        {isLoadingAccounts ? (
          <Flex justify="center" py={10}>
            <Spinner size="xl" />
          </Flex>
        ) : accounts && accounts.length > 0 ? (
          <SimpleGrid columns={{ base: 1, md: 2, lg: 3 }} gap={4}>
            {accounts.map((account) => (
              <CardRoot key={account.id}>
                <CardBody>
                  <Flex align="center">
                    <Icon
                      as={getAccountIcon(account.type)}
                      boxSize={6}
                      color="blue.500"
                      mr={3}
                    />
                    <Box>
                      <Text fontWeight="bold">{account.name}</Text>
                      <Text fontSize="sm" color="gray.500">
                        {account.mask ? `••••${account.mask}` : ""}
                        {account.subtype ? ` • ${account.subtype}` : ""}
                      </Text>
                    </Box>
                  </Flex>
                </CardBody>
              </CardRoot>
            ))}
          </SimpleGrid>
        ) : (
          <Box p={5} borderWidth="1px" borderRadius="lg" textAlign="center">
            <Text>
              No accounts connected yet. Click the button above to get started.
            </Text>
          </Box>
        )}
      </VStack>
    </Box>
  );
}
