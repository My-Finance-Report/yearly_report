import { Box, Button, Link, Text, VStack } from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/contact-me")({
  component: ContactMe,
})

function ContactMe() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6} textAlign="center">
      <VStack spaceY={4} align="center">
        <Text fontSize="lg">
          Hey, thanks for checking out My Financé! If you have questions,
          feedback, or just want to chat, I’d love to hear from you.
        </Text>

        <VStack spaceY={3} align="center">
          <Link href="mailto:mcarroll1220@gmail.com">
            <Button>mcarroll1220@gmail.com</Button>
          </Link>

          <Link target="_blank" href="https://cal.com/matt-carroll">
            <Button>Put time on my calendar</Button>
          </Link>
        </VStack>
      </VStack>
    </Box>
  )
}
