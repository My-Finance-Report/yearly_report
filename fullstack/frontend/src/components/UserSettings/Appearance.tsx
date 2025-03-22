import { ColorModeIcon, useColorMode } from "@/components/ui/color-mode"
import { Container, Heading, Stack } from "@chakra-ui/react"

import { Radio, RadioGroup } from "@/components/ui/radio"

const Appearance = () => {
  const { colorMode, toggleColorMode } = useColorMode()


  return (
    <>
      <Container maxW="full">
        <Heading size="sm" py={4}>
          Appearance
        </Heading>
        <RadioGroup onChange={toggleColorMode} value={colorMode}>
          <Stack>
            <ColorModeIcon />
            <Radio value="light" colorScheme="teal">
              Light Mode
            </Radio>
            <Radio value="dark" colorScheme="teal">
              Dark Mode
            </Radio>
          </Stack>
        </RadioGroup>
      </Container>
    </>
  )
}
export default Appearance
