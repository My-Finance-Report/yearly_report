import { Box, Heading, Text, VStack } from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/change-log")({
  component: ChangeLogPage,
})

const changes = [
  {
    date: "2025-04-07",
    changes: ["Added support for two-factor authentication", "Updated FAQ"],
  },
]

export function ChangeLogPage() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6}>
      <Heading as="h1" mb={6}>
        Change Log
      </Heading>

      <VStack spaceY={6} align="start">
        {changes.map((change, index) => (
          <Box key={index}>
            <Heading as="h3" size="md">
              {change.date}
            </Heading>
            {change.changes.map((change, index) => (
              <Text key={index}> - {change}</Text>
            ))}
          </Box>
        ))}
      </VStack>
    </Box>
  )
}
