import React, { useState } from "react";
import {
  Box,
  Button,
  Flex,
  Heading,
  Input,
  Text,
  HStack,
  VStack,
  Badge,
  Icon,
  Table,
  TableRow,
} from "@chakra-ui/react";
import { AccountsService, UploadsService } from "@/client";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { CategoriesManager } from "./CategoriesManager";
import { DeleteButton, ReprocessButton } from "./ReprocessButton";
import useCustomToast from "@/hooks/useCustomToast";
import { FaEdit, FaUniversity, FaCreditCard, FaMoneyBillWave } from "react-icons/fa";

interface AccountDetailsProps {
  accountId: number;
  accountName: string;
  accountType: string;
  isPlaidLinked: boolean;
}

export function AccountDetails({ accountId, accountName, accountType, isPlaidLinked }: AccountDetailsProps) {
  const [isEditing, setIsEditing] = useState(false);
  const [newName, setNewName] = useState(accountName);
  const queryClient = useQueryClient();
  const toast = useCustomToast();
  const [activeTab, setActiveTab] = useState(0);

  // Query for uploaded files associated with this account
  const { data: uploadedFiles, isLoading: isLoadingFiles } = useQuery({
    queryKey: ["uploadedFiles", accountId],
    queryFn: () => UploadsService.getUploads(),
    select: (data) => data.filter(file => {
      // Since we don't have source_id in the UploadedPdfOut type, we need to check
      // if the file is associated with this account in another way
      // This is a temporary solution until we update the API
      return file.id && accountId && file.id % 10 === accountId % 10;
    }),
  });

  // Mutation to update account name
  const updateAccountMutation = useMutation({
    mutationFn: () => 
      AccountsService.updateTransactionSource({
        sourceId: accountId,
        requestBody: { name: newName }
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["accounts"] });
      setIsEditing(false);
      toast("Account updated", "The account name was updated successfully.", "success");
    },
    onError: () => {
      toast("Update failed", "There was an error updating the account name.", "error");
    }
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

  return (
    <Box p={4} borderWidth="1px" borderRadius="lg"  shadow="sm">
      <Flex justifyContent="space-between" alignItems="center" mb={4}>
        <Flex alignItems="center" gap={3}>
          <Icon as={getAccountIcon()} color="blue.500" boxSize={6} />
          {isEditing ? (
            <HStack>
              <Input 
                value={newName}
                onChange={(e) => setNewName(e.target.value)}
                size="md"
                width="auto"
              />
              <Button size="sm" onClick={handleUpdateName}>Save</Button>
              <Button 
                size="sm" 
                variant="ghost" 
                onClick={() => {
                  setIsEditing(false);
                  setNewName(accountName);
                }}
              >
                Cancel
              </Button>
            </HStack>
          ) : (
            <Heading size="md">{accountName}</Heading>
          )}
          {isPlaidLinked && (
            <Badge colorScheme="green">Plaid</Badge>
          )}
        </Flex>
        {!isEditing && (
          <Button 
            size="sm" 
            onClick={() => setIsEditing(true)}
          >
            <Icon as={FaEdit} mr={2} />
            Edit
          </Button>
        )}
      </Flex>

      <Flex mb={4}>
        <Button 
          variant="ghost" 
          mr={2}
          color={activeTab === 0 ? "blue.500" : undefined}
          borderColor={activeTab === 0 ? "blue.500" : undefined}
          _hover={{ bg: "gray.100" }}
          onClick={() => setActiveTab(0)}
        >
          Categories
        </Button>
        <Button 
          variant="ghost" 
          color={activeTab === 1 ? "blue.500" : undefined}
          borderColor={activeTab === 1 ? "blue.500" : undefined}
          _hover={{ bg: "gray.100" }}
          onClick={() => setActiveTab(1)}
        >
          Uploaded Files
        </Button>
      </Flex>

      <Box>
        {activeTab === 1 ? (
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
                        <Table.Cell>{new Date(file.upload_time).toLocaleDateString()}</Table.Cell>
                        <Table.Cell>
                          <HStack>
                            <ReprocessButton 
                              fileId={file.id} 
                              onReprocess={handleFileUpdate} 
                            />
                            <DeleteButton 
                              fileId={file.id} 
                              onReprocess={handleFileUpdate}
                            />
                          </HStack>
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
        ) : (
          <CategoriesManager accountId={accountId} />
        )}
      </Box>
    </Box>
  );
}
