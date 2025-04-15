import { Flex } from "@chakra-ui/react"
import { SavedFilterControls } from "@/components/Common/SavedFilterControls"
import { Box } from "@chakra-ui/react"

export function NonPowerUserButtons() {
  return (
    <Box>
      <Flex justifyContent="space-between" alignItems="center">
        <SavedFilterControls />
      </Flex>
    </Box>
  )
}
