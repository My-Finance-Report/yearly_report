import {
  type UploadsDeleteFileResponse,
  type UploadsReprocessFileResponse,
  UploadsService,
} from "@/client"
import useCustomToast from "@/hooks/useCustomToast"
import { Button } from "@chakra-ui/react"

interface ReprocessButtonProps {
  jobId: number
  onReprocess: (response: UploadsReprocessFileResponse) => void
  disabled?: boolean
}

export function ReprocessButton({
  jobId,
  onReprocess,
  disabled = false,
}: ReprocessButtonProps) {
  const showToast = useCustomToast()
  const handleReprocess = async () => {
    try {
      const response: UploadsReprocessFileResponse =
        await UploadsService.reprocessFile({ jobId })
      showToast("File reprocessing!", "", "success")
      onReprocess(response)
    } catch {
      showToast("File failed to reprocess!", "", "error")
    }
  }

  return (
    <Button
      size="sm"
      colorScheme="blue"
      onClick={handleReprocess}
      disabled={disabled}
    >
      Reprocess
    </Button>
  )
}

export function DeleteButton({
  fileId,
  onReprocess,
  disabled = false,
}: {
  fileId: number
  onReprocess: (response: UploadsDeleteFileResponse) => void
  disabled?: boolean
}) {
  const handleReprocess = async () => {
    try {
      const response: UploadsDeleteFileResponse =
        await UploadsService.deleteFile({ fileId })
      onReprocess(response)
    } catch (error) {
      console.error("Failed to delete file:", error)
    }
  }

  return (
    <Button
      size="sm"
      colorScheme="red"
      onClick={handleReprocess}
      disabled={disabled}
    >
      Delete
    </Button>
  )
}
