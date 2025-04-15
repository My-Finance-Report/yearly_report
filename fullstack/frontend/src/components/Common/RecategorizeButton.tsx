import { AccountsService } from "@/client"
import useCustomToast from "@/hooks/useCustomToast"
import { Button, Icon } from "@chakra-ui/react"
import React from "react"
import { FaSync } from "react-icons/fa"

interface RecategorizeButtonProps {
  sourceId: number
  onRecategorize?: () => void
  disabled?: boolean
}

export function RecategorizeButton({
  sourceId,
  onRecategorize,
  disabled = false,
}: RecategorizeButtonProps) {
  const showToast = useCustomToast()

  const handleRecategorize = async () => {
    try {
      await AccountsService.triggerRecategorization({ sourceId })
      showToast(
        "Recategorization started",
        "Transactions are being recategorized in the background.",
        "success",
      )
      if (onRecategorize) {
        onRecategorize()
      }
    } catch (error) {
      showToast(
        "Recategorization failed",
        "There was an error starting the recategorization process.",
        "error",
      )
      console.error("Failed to recategorize:", error)
    }
  }

  return (
    <Button
      size="sm"
      colorScheme="teal"
      onClick={handleRecategorize}
      disabled={disabled}
    >
      <Icon as={FaSync} mr={2} />
      Recategorize
    </Button>
  )
}
