import { useColorMode } from "@/components/ui/color-mode"
import { Badge, Container, Heading, Stack } from "@chakra-ui/react"

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
            <Radio value="light" colorScheme="teal">
              Light Mode
              <Badge ml="1" colorScheme="teal">
                Default
              </Badge>
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
