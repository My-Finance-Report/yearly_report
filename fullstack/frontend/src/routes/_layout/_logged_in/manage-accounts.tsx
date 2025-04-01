import { AccountsService, UploadsService } from "@/client"
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
  Tabs,
  DialogRoot,
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogPositioner,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import { AccountDetails } from "@/components/Common/AccountDetails"
import { FaFileUpload, FaPlus, FaUniversity } from "react-icons/fa"
import { isLoggedIn } from "@/hooks/useAuth"
import FileDropzone from "@/components/Common/Dropzone"

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

  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const toast = useCustomToast()
  const { open: isOpen, onOpen, onClose } = useDisclosure()

  // Mutation to upload files
  const uploadMutation = useMutation({
    mutationFn: (files: File[]) => {
      // The Body_uploads_upload_files type only expects files
      const data = { formData: { files } };
      return UploadsService.uploadFiles(data);
    },
    onSuccess: () => {
      toast("Files uploaded", "The files were processed successfully.", "success");
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] });
      onClose();
    },
    onError: () => {
      toast("Upload failed", "There was an error uploading the files.", "error");
    },
  });

  const handleUpload = (files: File[]) => {
    if (files.length > 0) {
      uploadMutation.mutate(files);
    }
  };

  // Check if an account is linked to Plaid
  const isPlaidLinked = (accountId: number) => {
    // Check if the account ID is from Plaid based on the account data
    const account = accounts?.find(a => a.id === accountId);
    return account?.name.toLowerCase().includes('plaid') || false;
  }

  // Get account type
  const getAccountType = () => {
    return "depository"
  }

  if (isError) {
    return (
      <Container maxW="full">
        <Text>Failed to load accounts. Please try again.</Text>
      </Container>
    )
  }

  return (
    <Container mt={8} maxW="container.xl">
      <Flex justifyContent="space-between" alignItems="center" mb={6}>
        <Flex gap={2}>
      <Button
        onClick={() => navigate({ to: "/plaid" })}
      >
        <Flex align="center">
          <Icon as={FaUniversity} mr={2} />
          <Text>Connect Bank</Text>
        </Flex>
      </Button>
          <Button 
            onClick={onOpen}
          >
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
        <Tabs.Root 
          variant="plain" 
          defaultValue="0"
        >
          <Tabs.List>
            {accounts.map((account, index) => (
              <Tabs.Trigger p={8} key={account.id} value={index.toString()}>
                {account.name}
              </Tabs.Trigger>
            ))}
            <Tabs.Indicator />
          </Tabs.List>

          {accounts.map((account, index) => (
            <Tabs.Content key={account.id} value={index.toString()}>
              <AccountDetails 
                accountId={account.id} 
                accountName={account.name} 
                accountType={getAccountType()}
                isPlaidLinked={isPlaidLinked(account.id)}
              />
            </Tabs.Content>
          ))}
        </Tabs.Root>
      ) : (
        <Box textAlign="center" p={10} borderWidth="1px" borderRadius="lg">
          <VStack align="stretch" gap={4}>
            <Text fontSize="lg">You don't have any accounts yet.</Text>
            <Text>Create a new account or link your bank to get started.</Text>
            <Flex gap={2} mt={4}>
              <Button 
                colorScheme="blue" 
                onClick={() => navigate({ to: "/plaid" })}
              >
                <Flex align="center">
                  <Icon as={FaPlus} mr={2} />
                  <Text>Connect Account</Text>
                </Flex>
              </Button>
              <Button 
                colorScheme="green" 
                onClick={() => navigate({ to: "/upload-files" })}
              >
                <Flex align="center">
                  <Icon as={FaFileUpload} mr={2} />
                  <Text>Upload Files</Text>
                </Flex>
              </Button>
            </Flex>
          </VStack>
        </Box>
      )}

      <DialogRoot open={isOpen} onOpenChange={onClose}>
        <DialogBackdrop />
        <DialogPositioner>
        <DialogContent  >
          <DialogHeader>
            <DialogTitle>Upload Files</DialogTitle>
            <DialogCloseTrigger />
          </DialogHeader>
          
          <DialogBody>
            <FileDropzone
              handleUpload={handleUpload}
              isLoading={uploadMutation.isPending}
            />
            
          <DialogCloseTrigger asChild>
            <Button onClick={onClose}>Cancel</Button>
          </DialogCloseTrigger>
          </DialogBody>
        </DialogContent>
  </DialogPositioner>
      </DialogRoot>
    </Container>
  )
}
