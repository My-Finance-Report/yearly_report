import { Box, Flex, useDisclosure } from "@chakra-ui/react"
import { useEffect } from "react"
import { OnboardDialogs } from "./OnboardModal/Onboarding"

export function NullState() {
  const {
    open: isOnboardOpen,
    setOpen: setIsOnboardOpen,
    onClose,
  } = useDisclosure()

  useEffect(() => {
    setIsOnboardOpen(true)
  }, [])

  return (
    <Flex direction="column" alignItems="center" justifyContent="center">
      <Box maxWidth="400px">
        <OnboardDialogs
          isOnboardOpen={isOnboardOpen}
          onOnboardClose={onClose}
          isDialog={false}
        />
      </Box>
    </Flex>
  )
}
