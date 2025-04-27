import { AccountsService, UploadsService } from "@/client";
import useCustomToast from "@/hooks/useCustomToast";
import {
  Badge,
  Box,
  Button,
  Flex,
  HStack,
  Heading,
  Icon,
  Input,
  Table,
  Text,
  VStack,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { useState, useEffect } from "react";
import {
  FaCreditCard,
  FaEdit,
  FaMoneyBillWave,
  FaUniversity,
} from "react-icons/fa";
import { ArchiveButton } from "./ArchiveButton";
import { UploadButton } from "./UploadButton";
import { CategoriesManager } from "./CategoriesManager";
import { RecategorizeButton } from "./RecategorizeButton";
import { DeleteButton, ReprocessButton } from "./ReprocessButton";

import { useIsMobile } from "@/hooks/useIsMobile";
import PlaidSyncStatus from "./PlaidSyncStatus";

interface AccountDetailsProps {
  accountId: number;
  accountName: string;
  accountType: string;
  isPlaidLinked: boolean;
  isArchived?: boolean;
}

export function AccountDetails({
  accountId,
  accountName,
  accountType,
  isPlaidLinked,
  isArchived = false,
}: AccountDetailsProps) {
  const [isEditing, setIsEditing] = useState(false);
  const [newName, setNewName] = useState(accountName);
  const queryClient = useQueryClient();
  const toast = useCustomToast();
  const [activeTab, setActiveTab] = useState(0);

  useEffect(() => {
    if (!isEditing) {
      setNewName(accountName);
    }
  }, [accountName, isEditing]);

  const { data: uploadedFiles, isLoading: isLoadingFiles } = useQuery({
    queryKey: ["uploadedFiles", accountId],
    queryFn: () => UploadsService.getUploads(),
    select: (data) =>
      data.filter((file) => {
        return file.transaction_source_id === accountId;
      }),
  });

  const updateAccountMutation = useMutation({
    mutationFn: () =>
      AccountsService.updateTransactionSource({
        sourceId: accountId,
        requestBody: { name: newName },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["accounts"] });
      setIsEditing(false);
      toast(
        "Account updated",
        "The account name was updated successfully.",
        "success",
      );
    },
    onError: () => {
      toast(
        "Update failed",
        "There was an error updating the account name.",
        "error",
      );
    },
  });

  const handleUpdateName = () => {
    if (newName.trim() && newName !== accountName) {
      updateAccountMutation.mutate();
    } else {
      setIsEditing(false);
      setNewName(accountName);
    }
  };

  const handleFileUpdate = () => {
    queryClient.invalidateQueries({ queryKey: ["uploadedFiles", accountId] });
  };

  const getAccountIcon = () => {
    switch (accountType) {
      case "credit":
        return FaCreditCard;
      case "investment":
        return FaMoneyBillWave;
      default:
        return FaUniversity;
    }
  };

  const getAccountTypeColor = () => {
    switch (accountType) {
      case "credit":
        return "red";
      case "investment":
        return "blue";
      default:
        return "green";
    }
  };

  const getAccountTypeLabel = () => {
    switch (accountType) {
      case "credit":
        return "Credit";
      case "investment":
        return "Investment";
      default:
        return "Bank";
    }
  };

  const isMobile = useIsMobile();

  return (
    <Box
      p={4}
      borderWidth="1px"
      borderRadius="lg"
      shadow="sm"
      position="relative"
      bg={isArchived ? "gray.50" : undefined}
      _dark={{
        bg: isArchived ? "gray.800" : undefined,
      }}
    >
      {isArchived && (
        <Badge
          position="absolute"
          top={2}
          left={2}
          colorScheme="red"
          variant="solid"
        >
          Archived
        </Badge>
      )}
      <Flex justifyContent="space-between" alignItems="center" mb={4}>
        <Flex alignItems="center" gap={3} mt={isArchived ? 8 : 0}>
          <Icon
            as={getAccountIcon()}
            color={isArchived ? "gray.500" : "blue.500"}
            boxSize={6}
          />
          {isEditing ? (
            <HStack>
              <Input
                value={newName}
                onChange={(e) => setNewName(e.target.value)}
                size="md"
                width="400px"
                disabled={isArchived}
              />
              <Button
                size="sm"
                onClick={handleUpdateName}
                disabled={isArchived}
              >
                Save
              </Button>
              <Button
                size="sm"
                variant="ghost"
                onClick={() => {
                  setIsEditing(false);
                  setNewName(accountName);
                }}
                disabled={isArchived}
              >
                Cancel
              </Button>
            </HStack>
          ) : (
            <Flex alignItems="center">
              <Heading
                size="md"
                color={isArchived ? "gray.500" : undefined}
                _dark={{ color: isArchived ? "gray.400" : undefined }}
              >
                {accountName}
              </Heading>
              <Button
                aria-label="Edit account name"
                size="xs"
                variant="ghost"
                ml={2}
                onClick={() => setIsEditing(true)}
                disabled={isArchived}
              >
                <FaEdit />
              </Button>
            </Flex>
          )}
          {isPlaidLinked && (
            <Badge
              variant="outline"
              mr={1}
              colorPalette={isArchived ? "gray" : "blue"}
            >
              Connected
            </Badge>
          )}
          <Badge
            variant="outline"
            colorPalette={isArchived ? "gray.500" : getAccountTypeColor()}
            _dark={{ color: isArchived ? "gray.400" : getAccountTypeColor() }}
          >
            {getAccountTypeLabel()}
          </Badge>
        </Flex>
        <Flex gap={2}>
          {!isPlaidLinked && <UploadButton />}
          <RecategorizeButton sourceId={accountId} disabled={isArchived} />
          <ArchiveButton sourceId={accountId} isArchived={isArchived} />
        </Flex>
      </Flex>

      <Flex mb={4}>
        <Button
          variant="ghost"
          mr={2}
          borderColor={activeTab === 0 ? "blue.500" : undefined}
          onClick={() => setActiveTab(0)}
          disabled={isArchived}
        >
          Categories
        </Button>
        {isPlaidLinked ? (
          <Button
            variant="ghost"
            borderColor={activeTab === 1 ? "blue.500" : undefined}
            onClick={() => setActiveTab(1)}
            disabled={isArchived}
          >
            Data Syncs
          </Button>
        ) : (
          <Button
            variant="ghost"
            color={activeTab === 1 ? "blue.500" : undefined}
            borderColor={activeTab === 1 ? "blue.500" : undefined}
            onClick={() => setActiveTab(1)}
            disabled={isArchived}
          >
            Uploaded Files
          </Button>
        )}
      </Flex>

      <Box>
        {activeTab === 1 && !isPlaidLinked ? (
          <Box>
            {isLoadingFiles ? (
              <Text>Loading files...</Text>
            ) : uploadedFiles && uploadedFiles.length > 0 ? (
              <VStack align="stretch" gap={4}>
                <Text fontWeight="bold">Uploaded Files</Text>
                <Table.Root variant="outline">
                  <Table.Header>
                    <Table.Row>
                      <Table.Cell>Filename</Table.Cell>
                      <Table.Cell>Upload Date</Table.Cell>
                      <Table.Cell>Actions</Table.Cell>
                    </Table.Row>
                  </Table.Header>
                  <Table.Body>
                    {uploadedFiles.map((file) => (
                      <Table.Row key={file.id}>
                        <Table.Cell>{file.filename}</Table.Cell>
                        <Table.Cell>
                          {new Date(file.upload_time).toLocaleDateString()}
                        </Table.Cell>
                        <Table.Cell>
                          <Flex direction={isMobile ? "column" : "row"} gap={2}>
                            <ReprocessButton
                              jobId={file.id}
                              onReprocess={handleFileUpdate}
                              disabled={isArchived}
                            />
                            <DeleteButton
                              fileId={file.id}
                              onReprocess={handleFileUpdate}
                              disabled={isArchived}
                            />
                          </Flex>
                        </Table.Cell>
                      </Table.Row>
                    ))}
                  </Table.Body>
                </Table.Root>
              </VStack>
            ) : (
              <Text>No files uploaded yet.</Text>
            )}
          </Box>
        ) : activeTab === 0 ? (
          <CategoriesManager accountId={accountId} />
        ) : (
          isPlaidLinked && <PlaidSyncStatus accountId={accountId} />
        )}
      </Box>
    </Box>
  );
}
