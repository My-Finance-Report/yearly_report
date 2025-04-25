import { AccountsService } from "@/client";
import { Box, Flex, Icon, Spinner, Text, VStack } from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { format } from "date-fns";
import { FaExclamationTriangle, FaSync, FaUniversity } from "react-icons/fa";

const useSyncLogs = (accountId: number) => {
  return useQuery({
    queryKey: ["syncLogs", accountId],
    queryFn: async () => {
      const response = await AccountsService.getAccountSyncLogs({
        sourceId: accountId,
        limit: 1000,
      });
      if (!response) {
        throw new Error("Failed to fetch sync logs");
      }
      return response;
    },
    enabled: !!accountId,
  });
};

interface PlaidSyncStatusProps {
  accountId: number;
}

export default function PlaidSyncStatus({ accountId }: PlaidSyncStatusProps) {
  const { data: syncLogs, isLoading, isError } = useSyncLogs(accountId);

  if (isLoading) {
    return (
      <Box p={4} borderWidth="1px" borderRadius="md">
        <Flex align="center" gap={3}>
          <Spinner size="sm" color="blue.500" />
          <Text>Loading sync information...</Text>
        </Flex>
      </Box>
    );
  }

  if (isError || !syncLogs || syncLogs.length === 0) {
    return (
      <Box p={4} borderWidth="1px" borderRadius="md">
        <Flex align="center" gap={3}>
          <Icon as={FaUniversity} color="blue.500" boxSize={5} />
          <VStack align="start" gap={1}>
            <Text fontWeight="medium">Data Syncs Automatically</Text>
            <Text fontSize="sm">
              This account is connected to Plaid and transactions are synced
              daily.
              {isError && (
                <Text color="red.500" fontSize="xs" mt={1}>
                  <Icon as={FaExclamationTriangle} mr={1} />
                  Unable to fetch sync history.
                </Text>
              )}
            </Text>
          </VStack>
        </Flex>
      </Box>
    );
  }

  const lastSync = syncLogs[0];

  const timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;

  const lastSyncDate = new Date(lastSync.created_at);
  const formattedLocalTime = format(lastSyncDate, "MMM d, yyyy h:mm a");

  const totalTransactions = syncLogs.reduce((total: number, log) => {
    return (
      total +
      (log.added_count || 0) +
      (log.modified_count || 0) +
      (log.removed_count || 0)
    );
  }, 0);

  const totalSyncs = syncLogs.length;

  return (
    <Box p={4} borderWidth="1px" borderRadius="md">
      <Flex align="center" gap={3}>
        <Icon as={FaUniversity} color="blue.500" boxSize={5} />
        <VStack align="start" gap={1}>
          <Flex align="center">
            <Text fontWeight="medium">Data Syncs Automatically</Text>
            <Flex align="center" ml={2} color="gray.500" cursor="help">
              <Icon as={FaSync} size="sm" mr={1} />
              <Text fontSize="xs">
                {formattedLocalTime} ({timeZone})
              </Text>
            </Flex>
          </Flex>
          <Text fontSize="sm">
            This account is connected to Plaid and transactions are synced
            daily.
          </Text>
          {lastSync.added_count !== null && (
            <Text fontSize="xs" color="gray.600">
              Last sync: {lastSync.added_count} added, {lastSync.modified_count}{" "}
              modified, {lastSync.removed_count} removed
            </Text>
          )}
          <Flex align="center" mt={1}>
            <Text fontSize="xs" color="gray.600">
              Total: {totalTransactions} transactions processed across{" "}
              {totalSyncs === 1000 ? "1,000+" : totalSyncs} syncs
            </Text>
          </Flex>
        </VStack>
      </Flex>
    </Box>
  );
}
