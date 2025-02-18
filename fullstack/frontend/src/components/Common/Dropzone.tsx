import {
    FileUploadDropzone,
    FileUploadList,
    FileUploadRoot,
  } from "@/components/ui/file-upload.tsx"
  
  export default function FileDropzone() {
    return (
      <FileUploadRoot maxW="xl" alignItems="stretch" maxFiles={10}>
        <FileUploadDropzone
          label="Drag and drop here to upload"
          description=".pdf, .csv up to 5MB"
        />
        <FileUploadList />
      </FileUploadRoot>
    )
  }