"use client";

import React, { useState } from "react";
import {
  Container,
  Heading,
  Box,
  Button,
  Table,
  TableHeader,
  TableBody,
  TableRow,
  TableColumnHeader,
  TableCell,
  Spinner,
  Text,
} from "@chakra-ui/react";
import { ReprocessButton } from "@/components/Common/ReprocessButton";
import FileDropzone from "@/components/Common/Dropzone";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { UploadsService } from "../../client";
import { isLoggedIn } from "../../hooks/useAuth";
import type { UploadedPdfOut } from "../../client";
import useCustomToast from "@/hooks/useCustomToast";

export const Route = createFileRoute("/_layout/upload-files")({
  component: UploadFiles,
});

function UploadFiles() {
  const toast = useCustomToast();
  const queryClient = useQueryClient();
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);

  const uploadMutation = useMutation<UploadedPdfOut[], Error, File[]>({
    mutationFn: (files: File[]) => {
      const data = { formData: { files } };
      return UploadsService.uploadFiles(data);
    },
    onSuccess: () => {
      toast("Files uploaded", "The files were processed successfully.", "success");
      queryClient.invalidateQueries({ queryKey: ["uploadedFiles"] });
      setSelectedFiles([]); // ✅ Clear selected files after upload
    },
    onError: () => {
      toast("Upload failed", "There was an error uploading the files.", "error");
    },
  });

  const { data, isLoading, error, refetch } = useQuery<UploadedPdfOut[], Error>({
    queryKey: ["uploadedFiles"],
    queryFn: () => UploadsService.getUploads(),
    enabled: isLoggedIn(),
  });

  const handleFileSelect = (files: File[]) => {
    setSelectedFiles(files);
  };

  const handleUpload = () => {
    if (selectedFiles.length > 0) {
      uploadMutation.mutate(selectedFiles);
    }
  };

  const handleJobUpdate = () => {
    refetch();
  };

  return (
    <Container maxW="full" py={8}>
      <Heading mb={6}>File Uploads</Heading>

      <Box mb={8} p={4} borderWidth={1} borderRadius="md">
        <Heading size="md" mb={4}>
          Upload New Files
        </Heading>

        <FileDropzone onFilesSelected={handleFileSelect} />

        <Button onClick={handleUpload} loading={uploadMutation.isLoading} mt={4}>
          Upload
        </Button>
      </Box>

      <Heading size="md" mb={4}>Previously Uploaded Files</Heading>

      {isLoading ? (
        <Spinner />
      ) : error ? (
        <Text color="red.500">Error loading files.</Text>
      ) : data && data.length > 0 ? (
        <Table.Root variant="outline">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Filename</TableColumnHeader>
              <TableColumnHeader>Upload Time</TableColumnHeader>
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
                    <TableCell>{new Date(pdf.upload_time).toLocaleString()}</TableCell>
                    <TableCell>{pdf.job?.status || "Unknown"}</TableCell>
                    <TableCell>
                      <ReprocessButton
                        jobId={pdf.job.id}
                        onReprocess={handleJobUpdate}
                      />
                    </TableCell>
                  </TableRow>
                )
            )}
          </TableBody>
        </Table.Root>
      ) : (
        <Text>No files uploaded yet.</Text>
      )}
    </Container>
  );
}

export default UploadFiles;
