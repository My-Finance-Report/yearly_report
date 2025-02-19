import { type UploadsReprocessFileResponse, UploadsService } from "@/client"
import { Button } from "@chakra-ui/react"

interface ReprocessButtonProps {
  jobId: number
  onReprocess: (response: UploadsReprocessFileResponse) => void
}

export function ReprocessButton({ jobId, onReprocess }: ReprocessButtonProps) {
  const handleReprocess = async () => {
    try {
      const response: UploadsReprocessFileResponse =
        await UploadsService.reprocessFile({ jobId })
      onReprocess(response)
    } catch (error) {
      console.error("Failed to reprocess file:", error)
    }
  }

  return (
    <Button size="sm" colorScheme="blue" onClick={handleReprocess}>
      Reprocess
    </Button>
  )
}
