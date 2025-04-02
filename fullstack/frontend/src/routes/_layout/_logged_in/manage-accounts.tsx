import { AccountsService, UploadsService, PlaidService } from "@/client"
import useCustomToast from "@/hooks/useCustomToast"
import {
  Container,
  Button,
  VStack,
  Text,
  Flex,
  Spinner,
  Box,
  useDisclosure,
  Icon,
  DialogRoot,
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogPositioner,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  SelectContent,
  SelectItem,
  createListCollection,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { AccountDetails } from "@/components/Common/AccountDetails"
import { FaFileUpload, FaPlus, FaUniversity } from "react-icons/fa"
import { isLoggedIn } from "@/hooks/useAuth"
import FileDropzone from "@/components/Common/Dropzone"
import { useCallback, useEffect, useState } from "react"
import { usePlaidLink } from "react-plaid-link"

export const Route = createFileRoute("/_layout/_logged_in/manage-accounts")({
  component: ManageAccounts,
})

function ManageAccounts() {
  const {
    data: accounts,
    isLoading: isLoadingAccounts,
    isError,
  } = useQuery({
    queryKey: ["accounts"],
    queryFn: AccountsService.getTransactionSources,
    enabled: isLoggedIn(),
  })

  const queryClient = useQueryClient()
  const toast = useCustomToast()
  const { open: isUploadOpen, onOpen: onUploadOpen, onClose: onUploadClose } = useDisclosure()
  const { open: isPlaidOpen, onOpen: onPlaidOpen, onClose: onPlaidClose } = useDisclosure()
  const [linkToken, setLinkToken] = useState<string | null>(null)
  const [selectedAccountIndex, setSelectedAccountIndex] = useState<number>(0)

  // Mutation to upload files
  const uploadMutation = useMutation({
    mutationFn: (files: File[]) => {
      // The Body_uploads_upload_files type only expects files
      const data = { formData: { files } }
      return UploadsService.uploadFiles(data)
    },
    onSuccess: () => {
      toast("Files uploaded", "The files were processed successfully.", "success")
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] })
      onUploadClose()
    },
    onError: () => {
      toast("Upload failed", "There was an error uploading the files.", "error")
    },
  })

  const handleUpload = (files: File[]) => {
    if (files.length > 0) {
      uploadMutation.mutate(files)
    }
  }

  // Create link token mutation
  const createLinkTokenMutation = useMutation({
    mutationFn: async () => {
      const response = await PlaidService.getLinkToken()
      return response
    },
    onSuccess: (data) => {
      setLinkToken(data.link_token)
    },
    onError: () => {
      toast("Error", "Could not create link token", "error")
    },
  })

  // Exchange public token for access token
  const exchangeTokenMutation = useMutation({
    mutationFn: async (public_token: string) => {
      const response = await PlaidService.exchangeToken({
        requestBody: {
          public_token,
        },
      })
      return response
    },
    onSuccess: () => {
      toast("Success", "Account connected successfully", "success")
      queryClient.invalidateQueries({ queryKey: ["accounts"] })
      onPlaidClose()
    },
    onError: () => {
      toast("Error", "Failed to connect account", "error")
    },
  })

  // Initialize Plaid Link
  const { open, ready } = usePlaidLink({
    token: linkToken,
    onSuccess: (public_token) => {
      exchangeTokenMutation.mutate(public_token)
      setLinkToken(null)
    },
    onExit: (err) => {
      if (err) {
        toast("Connection Error", err.display_message || "Error connecting to your bank", "error")
      }
      onPlaidClose()
    },
  })

  useEffect(() => {
    if (isPlaidOpen && !linkToken) {
      createLinkTokenMutation.mutate()
    }
  }, [isPlaidOpen, linkToken])

  const handleConnectPlaid = useCallback(() => {
    if (ready && linkToken) {
      open()
    } else {
      createLinkTokenMutation.mutate()
    }
  }, [ready, linkToken, open])

  // Check if an account is linked to Plaid
  const isPlaidLinked = (accountId: number) => {
    // Check if the account is connected to Plaid based on the is_plaid_connected field
    const account = accounts?.find((a) => a.id === accountId)
    return account?.is_plaid_connected || false
  }

  // Get account type
  const getAccountType = () => {
    return "depository"
  }

  const isPlaidLoading = createLinkTokenMutation.isPending || exchangeTokenMutation.isPending

  // Handle account selection change
  const handleAccountChange = (details: { value: string[] }) => {
    if (details.value.length > 0) {
      setSelectedAccountIndex(parseInt(details.value[0], 10));
    }
  };

  if (isError) {
    return (
      <Container maxW="full">
        <Text>Failed to load accounts. Please try again.</Text>
      </Container>
    )
  }

  return (
    <Container mt={8} maxW="container.xl" minH="75vh">
      <Flex justifyContent="space-between" alignItems="center" mb={6}>
        <Flex gap={2}>
          <Button onClick={onPlaidOpen}>
            <Flex align="center">
              <Icon as={FaUniversity} mr={2} />
              <Text>Connect Account</Text>
            </Flex>
          </Button>
            <Button onClick={onUploadOpen}>
              <Flex align="center">
                <Icon as={FaFileUpload} mr={2} />
                <Text>Upload Files</Text>
              </Flex>
            </Button>
        </Flex>
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
              placeholder="Select an account"
              size="lg"
              collection={createListCollection({
                items: accounts.map((account, index) => ({
                  label: account.name,
                  value: index.toString(),
                }))
              })}
              value={[selectedAccountIndex.toString()]}
              onValueChange={handleAccountChange}
            >
              <SelectTrigger>
                <SelectValueText placeholder="Select an account" />
              </SelectTrigger>
              <SelectContent>
                {accounts.map((account, index) => (
                  <SelectItem 
                    key={account.id} 
                    item={{ label: account.name, value: index.toString() }}
                  >
                    {account.name}
                  </SelectItem>
                ))}
              </SelectContent>
            </SelectRoot>
          </Box>

          {accounts[selectedAccountIndex] && (
            <AccountDetails
              accountId={accounts[selectedAccountIndex].id}
              accountName={accounts[selectedAccountIndex].name}
              accountType={getAccountType()}
              isPlaidLinked={isPlaidLinked(accounts[selectedAccountIndex].id)}
              isArchived={accounts[selectedAccountIndex].archived}
            />
          )}
        </Box>
      ) : (
        <Box textAlign="center" p={10} borderWidth="1px" borderRadius="lg">
          <VStack align="stretch" gap={4}>
            <Text fontSize="lg">You don't have any accounts yet.</Text>
            <Text>Create a new account or link your bank to get started.</Text>
            <Flex gap={2} mt={4}>
              <Button colorScheme="blue" onClick={onPlaidOpen}>
                <Flex align="center">
                  <Icon as={FaPlus} mr={2} />
                  <Text>Connect Account</Text>
                </Flex>
              </Button>
              <Button colorScheme="green" onClick={onUploadOpen}>
                <Flex align="center">
                  <Icon as={FaFileUpload} mr={2} />
                  <Text>Upload Files</Text>
                </Flex>
              </Button>
            </Flex>
          </VStack>
        </Box>
      )}

      <DialogRoot open={isUploadOpen} onOpenChange={onUploadClose}>
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Upload Files</DialogTitle>
              <DialogCloseTrigger />
            </DialogHeader>

            <DialogBody>
              <FileDropzone handleUpload={handleUpload} isLoading={uploadMutation.isPending} />

              <DialogCloseTrigger asChild>
                <Button onClick={onUploadClose}>Cancel</Button>
              </DialogCloseTrigger>
            </DialogBody>
          </DialogContent>
        </DialogPositioner>
      </DialogRoot>

      {/* Connect Plaid Dialog */}
      <DialogRoot open={isPlaidOpen} onOpenChange={onPlaidClose}>
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Connect Bank Account</DialogTitle>
              <DialogCloseTrigger />
            </DialogHeader>

            <DialogBody>
              <VStack spaceY={4} align="stretch" mb={4}>
                <Text>
                  Connect your bank account securely using Plaid. This will allow you to:
                </Text>
                <VStack align="start" pl={4}>
                  <Text>• Automatically import transactions</Text>
                  <Text>• Keep your account balance up-to-date</Text>
                  <Text>• Sync new transactions daily</Text>
                </VStack>
                <Text>
                  Your login credentials are never stored on our servers. All data is encrypted and securely transmitted.
                </Text>
                {accounts && accounts.length > 0 && (
                  <Text fontWeight="medium" color="blue.500">
                    Note: You currently have {accounts.length} account{accounts.length > 1 ? "s" : ""}.{" "}
                    {accounts.length >= 1 && !accounts.some((a) => isPlaidLinked(a.id)) && " You don't have any Plaid-connected accounts yet."}
                  </Text>
                )}
              </VStack>

              <Flex justify="space-between" mt={4}>
                <DialogCloseTrigger asChild>
                  <Button variant="outline">Cancel</Button>
                </DialogCloseTrigger>
                <Button colorScheme="blue" onClick={handleConnectPlaid} disabled={isPlaidLoading || !ready}>
                  {isPlaidLoading ? "Preparing..." : "Connect Bank Account"}
                </Button>
              </Flex>
            </DialogBody>
          </DialogContent>
        </DialogPositioner>
      </DialogRoot>
    </Container>
  )
}
