import {
  FileUploadDropzone,
  FileUploadList,
  FileUploadTrigger,
} from "@/components/ui/file-upload.tsx"
import {
  FileUploadHiddenInput,
  FileUploadRootProvider,
  HStack,
  useFileUpload,
} from "@chakra-ui/react"
import { Button, Stack } from "@chakra-ui/react"
import { useEffect } from "react"
import { HiUpload } from "react-icons/hi"

interface FileDropzoneProps {
  onFilesSelected?: React.Dispatch<React.SetStateAction<File[]>>
  handleUpload: (files: File[]) => void
  isLoading: boolean
}

export default function FileDropzone({
  onFilesSelected,
  handleUpload,
  isLoading,
}: FileDropzoneProps) {
  const fileUpload = useFileUpload({
    maxFiles: 50,
    maxFileSize: 5 * 1024 * 1024, // 5MB
    accept: [".pdf", ".csv"],
  })

  useEffect(() => {
    if (fileUpload.acceptedFiles.length > 0 && onFilesSelected) {
      onFilesSelected(fileUpload.acceptedFiles)
    }
  }, [fileUpload.acceptedFiles, onFilesSelected])

  const handleUploadClick = () => {
    if (fileUpload.acceptedFiles.length > 0) {
      handleUpload(fileUpload.acceptedFiles)
      fileUpload.clearFiles()
    }
  }

  return (
    <FileUploadRootProvider value={fileUpload} alignItems={"center"}>
      <Stack align="center">
        <FileUploadHiddenInput />

        <FileUploadDropzone
          label="Drag and drop here to upload"
          description=".pdf, .csv up to 5MB"
        />

        <FileUploadList clearable />

        <HStack flex={"row"} gap={4} mt={4}>
          <Button
            onClick={handleUploadClick}
            loading={isLoading}
            variant={"solid"}
            size="md"
          >
            Upload
          </Button>
          {fileUpload.acceptedFiles.length > 0 && (
            <FileUploadTrigger asChild>
              <Button variant="outline" size="md">
                <HiUpload /> Select More Files
              </Button>
            </FileUploadTrigger>
          )}
        </HStack>
      </Stack>
    </FileUploadRootProvider>
  )
}
