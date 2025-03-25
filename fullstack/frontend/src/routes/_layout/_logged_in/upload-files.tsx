"use client"

import FileDropzone from "@/components/Common/Dropzone"
import {
  DeleteButton,
  ReprocessButton,
} from "@/components/Common/ReprocessButton"
import useCustomToast from "@/hooks/useCustomToast"
import {
  Container,
  HStack,
  Button,
  Box,
  Flex,
  Spinner,
  Table,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRow,
  Text,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { createFileRoute, Link } from "@tanstack/react-router"
import { UploadsService } from "@/client"
import type { UploadedPdfOut } from "@/client"
import { isLoggedIn } from "@/hooks/useAuth"
import { useState } from "react"

interface SortConfig {
  keyExtractor: (obj: UploadedPdfOut) => string;
  direction: "asc" | "desc";
  columnName: string
}

export const Route = createFileRoute("/_layout/_logged_in/upload-files")({
  component: UploadFiles,
})

function UploadFiles() {
  const toast = useCustomToast()
  const queryClient = useQueryClient()

  const uploadMutation = useMutation<UploadedPdfOut[], Error, File[]>({
    mutationFn: (files: File[]) => {
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
    },
    onError: () => {
      toast("Upload failed", "There was an error uploading the files.", "error")
    },
  })

  const { data, isLoading, error, refetch } = useQuery<UploadedPdfOut[], Error>(
    {
      queryKey: ["uploadedFiles"],
      queryFn: () => UploadsService.getUploads(),
      enabled: isLoggedIn(),
    },
  )

  const handleUpload = (files: File[]) => {
    if (files.length > 0) {
      uploadMutation.mutate(files)
    }
  }

  const handleUpdate = () => {
    refetch()
  }

 const [sortConfig, setSortConfig] = useState<SortConfig>({ keyExtractor: (obj: UploadedPdfOut) => obj.nickname || "", direction: "asc", columnName: "nickname" });

  const sortedData = [...(data || [])].sort((a, b) => {
    const valueA = sortConfig.keyExtractor(a);
    const valueB = sortConfig.keyExtractor(b);

    if (valueA < valueB) return sortConfig.direction === "asc" ? -1 : 1;
    if (valueA > valueB) return sortConfig.direction === "asc" ? 1 : -1;
    return 0;
  });

  const handleSort = (getKey: (obj: UploadedPdfOut) => string, columnName: string) => {
    setSortConfig((prev) => ({
      keyExtractor: getKey,
      direction: prev.columnName === columnName ? (prev.direction === "asc" ? "desc" : "asc") : "asc",
      columnName
    }));
  };


  return (
    <Container maxW="large" py={8}>
      <FileDropzone
        handleUpload={handleUpload}
        isLoading={uploadMutation.isPending}
      />
      <Flex align="center" my={4} justifyContent="center">
        <Box flex="1" h="1px" bg="gray.200" maxWidth="20%" />
        <Text px={3} fontSize="sm" color="gray.500">OR</Text>
        <Box flex="1" h="1px" bg="gray.200" maxWidth="20%" />
      </Flex>
      <Flex justifyContent="center" direction='row'alignItems="center">
      <Text>
        <Button>
          <Link color="blue.500" to="/plaid">Link Account Directly</Link>
        </Button>
      </Text>
</Flex>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading files.</Text>
      ) : data && data.length > 0 ? (
        <Table.Root variant="outline" mt={24} rounded="md">
          <TableHeader>
            <TableRow>
              <TableColumnHeader cursor="pointer" onClick={() => handleSort((obj)=> obj.filename, "filename")}>
                Filename {sortConfig.columnName === "filename" && (sortConfig.direction === "asc" ? "▲" : "▼")}
              </TableColumnHeader>
              <TableColumnHeader cursor="pointer" onClick={() => handleSort((obj)=> obj.nickname || "", "nickname")}>
                Nickname {sortConfig.columnName === "nickname" && (sortConfig.direction === "asc" ? "▲" : "▼")}
              </TableColumnHeader>
              <TableColumnHeader cursor="pointer" onClick={() => handleSort((obj)=> obj.job?.status || "Unknown", "status")}>
                Status {sortConfig.columnName === "status" && (sortConfig.direction === "asc" ? "▲" : "▼")}
              </TableColumnHeader>
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {sortedData.map(
              (pdf) =>
                pdf.job && (
                  <TableRow key={pdf.id}>
                    <TableCell>{pdf.filename}</TableCell>
                    <TableCell>{pdf.nickname}</TableCell>
                    <TableCell>{pdf.job?.status || "Unknown"}</TableCell>
                    <TableCell>
                      <HStack gap={2}>
                        <ReprocessButton
                          jobId={pdf.job.id}
                          onReprocess={handleUpdate}
                        />
                        <DeleteButton
                          fileId={pdf.id}
                          onReprocess={handleUpdate}
                        />
                      </HStack>
                    </TableCell>
                  </TableRow>
                ),
            )}
          </TableBody>
        </Table.Root>
      ) : (
        <Text>No files uploaded yet.</Text>
      )}
    </Container>
  )
}

export default UploadFiles
