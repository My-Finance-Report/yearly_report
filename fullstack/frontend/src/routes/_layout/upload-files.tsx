"use client"

import FileDropzone from "@/components/Common/Dropzone"
import { ReprocessButton, DeleteButton } from "@/components/Common/ReprocessButton"
import useCustomToast from "@/hooks/useCustomToast"
import {
  Box,
  Button,
  Container,
  Heading,
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
import { createFileRoute } from "@tanstack/react-router"
import { useState } from "react"
import { UploadsService } from "../../client"
import type { UploadedPdfOut } from "../../client"
import { isLoggedIn } from "../../hooks/useAuth"

export const Route = createFileRoute("/_layout/upload-files")({
  component: UploadFiles,
})

function UploadFiles() {
  const toast = useCustomToast()
  const queryClient = useQueryClient()
  const [selectedFiles, setSelectedFiles] = useState<File[]>([])

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
      console.log("toasted")
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] })
      setSelectedFiles([])
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

  return (
    <Container maxW="large" py={8}>
      <FileDropzone
        handleUpload={handleUpload}
        isLoading={uploadMutation.isPending}
      />

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading files.</Text>
      ) : data && data.length > 0 ? (
        <Table.Root variant="outline" mt={24}>
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Filename</TableColumnHeader>
              <TableColumnHeader>Nickname</TableColumnHeader>
              <TableColumnHeader>Status</TableColumnHeader>
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {data.map(
              (pdf) =>
                pdf.job && (
                  <TableRow key={pdf.id}>
                    <TableCell>{pdf.filename}</TableCell>
                    <TableCell>{pdf.nickname}</TableCell>
                    <TableCell>{pdf.job?.status || "Unknown"}</TableCell>
                    <TableCell>
                      <ReprocessButton
                        jobId={pdf.job.id}
                        onReprocess={handleUpdate}
                      />
                  <DeleteButton
                        fileId={pdf.id}
                        onReprocess={handleUpdate}
                      />
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
