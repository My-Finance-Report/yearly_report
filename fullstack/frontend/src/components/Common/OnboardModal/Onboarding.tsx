import { PlaidService, UploadsService } from "@/client"
import FileDropzone from "@/components/Common/Dropzone"
import useCustomToast from "@/hooks/useCustomToast"
import {
  Box,
  Button,
  CloseButton,
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogHeader,
  DialogPositioner,
  DialogRoot,
  DialogTitle,
  Flex,
  Heading,
  Icon,
  Text,
  useDisclosure,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { useCallback, useEffect, useState } from "react"
import { FaFileUpload, FaUniversity } from "react-icons/fa"
import { usePlaidLink } from "react-plaid-link"

export function OnboardDialogs({
  isOnboardOpen,
  onOnboardClose,
  isDialog,
}: { isDialog: boolean; isOnboardOpen: boolean; onOnboardClose: () => void }) {
  const {
    open: isUploadOpen,
    onOpen: onUploadOpen,
    onClose: onUploadClose,
  } = useDisclosure()
  const {
    open: isPlaidOpen,
    onOpen: onPlaidOpen,
    onClose: onPlaidClose,
  } = useDisclosure()

  return (
    <>
      <PlaidOrUploadSelector
        isDialog={isDialog}
        isOnboardOpen={isOnboardOpen && !isPlaidOpen && !isUploadOpen}
        onPlaidOpen={onPlaidOpen}
        onUploadOpen={onUploadOpen}
        onOnboardClose={onOnboardClose}
      />
      <PlaidDialog isPlaidOpen={isPlaidOpen} onPlaidClose={onPlaidClose} />
      <UploadDialog isUploadOpen={isUploadOpen} onUploadClose={onUploadClose} />
    </>
  )
}

function PlaidDialog({
  isPlaidOpen,
  onPlaidClose,
}: { isPlaidOpen: boolean; onPlaidClose: () => void }) {
  const toast = useCustomToast()
  const queryClient = useQueryClient()
  const [linkToken, setLinkToken] = useState<string | null>(null)

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
        toast(
          "Connection Error",
          err.display_message || "Error connecting to your bank",
          "error",
        )
      }
      onPlaidClose()
    },
  })

  useEffect(() => {
    if (isPlaidOpen && !linkToken) {
      createLinkTokenMutation.mutate()
    }
  }, [isPlaidOpen, linkToken])

  const isPlaidLoading =
    createLinkTokenMutation.isPending || exchangeTokenMutation.isPending

  const handleConnectPlaid = useCallback(() => {
    if (ready && linkToken) {
      open()
    } else {
      createLinkTokenMutation.mutate()
    }
  }, [ready, linkToken, open])

  return (
    <DialogRoot
      open={isPlaidOpen}
      onOpenChange={onPlaidClose}
      placement={"center"}
    >
      <DialogBackdrop />
      <DialogPositioner>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Connect Bank Account</DialogTitle>
            <DialogCloseTrigger />
          </DialogHeader>
          <DialogBody>
            <Flex direction="column" gap={4} align="stretch" mt={4}>
              <Text>
                Connect your bank account securely using Plaid. This will allow
                you to:
              </Text>
              <Flex direction="column" align="start" pl={4}>
                <Text>• Automatically import transactions</Text>
                <Text>• Keep your account balance up-to-date</Text>
                <Text>• Sync new transactions daily</Text>
              </Flex>
              <Text>
                Your login credentials are never stored on our servers. All data
                is encrypted and securely transmitted.
              </Text>
            </Flex>

            <Flex justify="space-between" mt={4}>
              <DialogCloseTrigger asChild>
                <Button variant="outline">Cancel</Button>
              </DialogCloseTrigger>
              <Button
                colorScheme="blue"
                onClick={handleConnectPlaid}
                disabled={isPlaidLoading || !ready}
              >
                {isPlaidLoading ? "Preparing..." : "Connect Bank Account"}
              </Button>
            </Flex>
          </DialogBody>
        </DialogContent>
      </DialogPositioner>
    </DialogRoot>
  )
}

function UploadDialog({
  isUploadOpen,
  onUploadClose,
}: { isUploadOpen: boolean; onUploadClose: () => void }) {
  const toast = useCustomToast()
  const queryClient = useQueryClient()

  const uploadMutation = useMutation({
    mutationFn: (files: File[]) => {
      // The Body_uploads_upload_files type only expects files
      const data = { formData: { files } }
      return UploadsService.uploadFiles(data)
    },
    onSuccess: () => {
      toast(
        "Files uploaded",
        "The files were processed successfully.",
        "success",
      )
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

  return (
    <DialogRoot
      open={isUploadOpen}
      onOpenChange={onUploadClose}
      placement={"center"}
    >
      <DialogBackdrop />
      <DialogPositioner>
        <DialogContent>
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
              <Button onClick={onUploadClose}>Cancel</Button>
            </DialogCloseTrigger>
          </DialogBody>
        </DialogContent>
      </DialogPositioner>
    </DialogRoot>
  )
}

function PlaidOrUploadSelector({
  isDialog,
  isOnboardOpen,
  onUploadOpen,
  onPlaidOpen,
  onOnboardClose,
}: {
  isDialog: boolean
  onUploadOpen: () => void
  isOnboardOpen: boolean
  onPlaidOpen: () => void
  onOnboardClose: () => void
}) {
  if (isDialog) {
    return (
      <DialogRoot
        open={isOnboardOpen}
        onOpenChange={onOnboardClose}
        placement={"center"}
      >
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Add a New Account</DialogTitle>

              <DialogCloseTrigger asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </DialogCloseTrigger>
            </DialogHeader>
            <DialogBody>
              <AccountSelectionOptions
                onPlaidOpen={onPlaidOpen}
                onOnboardClose={onOnboardClose}
                onUploadOpen={onUploadOpen}
              />
            </DialogBody>
          </DialogContent>
        </DialogPositioner>
      </DialogRoot>
    )
  }

  return (
    <AccountSelectionOptions
      onPlaidOpen={onPlaidOpen}
      onOnboardClose={onOnboardClose}
      onUploadOpen={onUploadOpen}
    />
  )
}

function AccountSelectionOptions({
  onPlaidOpen,
  onOnboardClose,
  onUploadOpen,
}: {
  onPlaidOpen: () => void
  onOnboardClose: () => void
  onUploadOpen: () => void
}) {
  return (
    <Flex direction="column" gap={4} align="stretch">
      <Box
        p={4}
        borderWidth="1px"
        borderRadius="md"
        cursor="pointer"
        onClick={() => {
          onOnboardClose()
          onPlaidOpen()
        }}
      >
        <Flex align="center">
          <Icon as={FaUniversity} boxSize={6} color="blue.500" />
          <Box ml={4}>
            <Heading size="sm">Connect Bank Account</Heading>
            <Text fontSize="sm" mt={1}>
              Securely connect your bank account via Plaid for automatic
              transaction syncing. Your login credentials are never stored.
            </Text>
          </Box>
        </Flex>
      </Box>

      <Box borderBottomWidth="1px" my={2} />

      <Box
        p={4}
        borderWidth="1px"
        borderRadius="md"
        cursor="pointer"
        onClick={() => {
          onOnboardClose()
          onUploadOpen()
        }}
      >
        <Flex align="center">
          <Icon as={FaFileUpload} boxSize={6} color="green.500" />
          <Box ml={4}>
            <Heading size="sm">Upload Account Statements</Heading>
            <Text fontSize="sm" mt={1}>
              Upload CSV or PDF statements from your bank. This is a good option
              if you prefer not to connect your account directly or if your bank
              isn't supported.
            </Text>
          </Box>
        </Flex>
      </Box>
    </Flex>
  )
}
