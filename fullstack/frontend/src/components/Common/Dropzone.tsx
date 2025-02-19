import {
  FileUploadDropzone,
  FileUploadList,
  FileUploadTrigger,
} from "@/components/ui/file-upload.tsx";
import {
  FileUploadHiddenInput,
  FileUploadRootProvider,
  useFileUpload,
} from "@chakra-ui/react";
import { Button, Stack } from "@chakra-ui/react";
import { useEffect } from "react";
import { HiUpload } from "react-icons/hi";

interface FileDropzoneProps {
  onFilesSelected: React.Dispatch<React.SetStateAction<File[]>>
}

export default function FileDropzone({ onFilesSelected }: FileDropzoneProps) {
  const fileUpload = useFileUpload({
    maxFiles: 50,
    maxFileSize: 5 * 1024 * 1024, // 5MB
    accept: [".pdf", ".csv"],
  });

  useEffect(() => {
    if (fileUpload.acceptedFiles.length > 0) {
      onFilesSelected(fileUpload.acceptedFiles);
    }
    console.log(fileUpload.acceptedFiles)
  }, [fileUpload.acceptedFiles, onFilesSelected]);

  return (
    <FileUploadRootProvider value={fileUpload} alignItems={'stretch'}>
      <Stack align="center" w="full">
        <FileUploadHiddenInput />
        
        <FileUploadDropzone
          label="Drag and drop here to upload"
          description=".pdf, .csv up to 5MB"
        />

        <FileUploadList clearable />

        <FileUploadTrigger asChild>
          <Button variant="outline" size="sm">
            <HiUpload /> Select Files
          </Button>
        </FileUploadTrigger>
      </Stack>
    </FileUploadRootProvider>
  );
}
