import { Flex } from "@chakra-ui/react"
import { SavedFilterControls } from "@/components/Common/SavedFilterControls"
import { Box } from "@chakra-ui/react"

export function NonPowerUserButtons() {
  return (
    <Box>
      <Flex justifyContent="space-between" alignItems="center" mb={4}>
        <SavedFilterControls />
      </Flex>
    </Box>
  )
}
