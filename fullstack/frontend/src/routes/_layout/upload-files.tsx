import React, { useState } from "react"
import {
  Container,
  Heading,
  Box,
  Button,
  Input,
  Table,
  TableContainer,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Spinner,
  Text,
  useToast,
} from "@chakra-ui/react"
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { UploadsService } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

import type { UploadedPdfOut } from "../../client"

export const Route = createFileRoute("/_layout/upload-files")({
  component: UploadFiles,
})

function UploadFiles() {
  const toast = useToast()
  const queryClient = useQueryClient()
  // Now selectedFiles is an array of File objects.
  const [selectedFiles, setSelectedFiles] = useState<File[]>([])

  // Mutation for uploading multiple files.
  const uploadMutation = useMutation<UploadedPdfOut[], Error, File[]>({
    mutationFn: (files: File[]) => {
      const data = { formData: {files} }
      return UploadsService.uploadFiles(data)
    },
    onSuccess: () => {
      toast({
        title: "Files uploaded",
        description: "The files were processed successfully.",
        status: "success",
        duration: 5000,
        isClosable: true,
      })
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] })
    },
    onError: () => {
      toast({
        title: "Upload failed",
        description: "There was an error uploading the files.",
        status: "error",
        duration: 5000,
        isClosable: true,
      })
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] })
    },
  })

  // Query to fetch uploaded files.
  const { data, isLoading, error } = useQuery<UploadedPdfOut[], Error>({
    queryKey: ["uploadedFiles"],
    queryFn: () => UploadsService.getUploads(),
    enabled: isLoggedIn(),
  })

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      // Convert FileList to an array.
      setSelectedFiles(Array.from(e.target.files))
    }
  }

  const handleUpload = () => {
    if (selectedFiles.length > 0) {
      uploadMutation.mutate(selectedFiles)
    }
  }

  return (
    <Container maxW="full" py={8}>
      <Heading mb={6}>File Uploads</Heading>

      {/* Upload form */}
      <Box mb={8} p={4} borderWidth={1} borderRadius="md">
        <Heading size="md" mb={4}>
          Upload New Files
        </Heading>
        <Input type="file" multiple onChange={handleFileChange} mb={4} />
        <Button onClick={handleUpload} isLoading={uploadMutation.isLoading}>
          Upload
        </Button>
      </Box>

      <Heading size="md" mb={4}>
        Previously Uploaded Files
      </Heading>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading files.</Text>
      ) : data && data.length > 0 ? (
        <TableContainer>
          <Table variant="simple">
            <Thead>
              <Tr>
                <Th>ID</Th>
                <Th>Filename</Th>
                <Th>Upload Time</Th>
                <Th>Archived</Th>
              </Tr>
            </Thead>
            <Tbody>
              {data.map((pdf) => (
                <Tr key={pdf.id}>
                  <Td>{pdf.id}</Td>
                  <Td>{pdf.filename}</Td>
                  <Td>{new Date(pdf.upload_time).toLocaleString()}</Td>
                  <Td>{pdf.archived ? "Yes" : "No"}</Td>
                </Tr>
              ))}
            </Tbody>
          </Table>
        </TableContainer>
      ) : (
        <Text>No files uploaded yet.</Text>
      )}
    </Container>
  )
}

export default UploadFiles
