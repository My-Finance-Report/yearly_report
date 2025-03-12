import {
  type UploadsDeleteFileResponse,
  type UploadsReprocessFileResponse,
  UploadsService,
} from "@/client"
import useCustomToast from "@/hooks/useCustomToast";
import { Button } from "@chakra-ui/react"

interface ReprocessButtonProps {
  jobId: number
  onReprocess: (response: UploadsReprocessFileResponse) => void
}

export function ReprocessButton({ jobId, onReprocess }: ReprocessButtonProps) {

  const showToast = useCustomToast();
  const handleReprocess = async () => {
    try {
      const response: UploadsReprocessFileResponse =
        await UploadsService.reprocessFile({ jobId })
      showToast("File reprocessing!","", "success")
      onReprocess(response)
    } catch {
      showToast("File failed to reprocess!","", "error")
    }
  }

  return (
    <Button size="sm" colorScheme="blue" onClick={handleReprocess}>
      Reprocess
    </Button>
  )
}

export function DeleteButton({
  fileId,
  onReprocess,
}: {
  fileId: number
  onReprocess: (response: UploadsDeleteFileResponse) => void
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
      color="red.400"
      variant="outline"
      onClick={handleReprocess}
    >
      Delete File and Transactions
    </Button>
  )
}
