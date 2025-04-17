import { Box, Heading, Text, VStack } from "@chakra-ui/react"
import { createFileRoute } from "@tanstack/react-router"

export const Route = createFileRoute("/_layout/privacy")({
  component: Privacy,
})

function Privacy() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6}>
      <Heading as="h1" mb={6}>
        Privacy Policy
      </Heading>

      <VStack spaceY={6} align="start">
        <Box>
          <Heading as="h3" size="md">
            1. What Data Do We Collect?
          </Heading>
          <Text>
            We only collect the data you upload, such as bank statements, in
            order to provide insights into your spending habits. The only
            anayltics tool we use is Goat Counter, which simply counts the
            pageviews of each page in the app.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            2. How Is Your Data Used?
          </Heading>
          <Text>
            Your uploaded data is processed and used solely for categorization
            and financial analysis within the app. We do not share your data
            with anyone, beyond the llm provider, which currently is OpenAI. We
            do not sell your data.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            3. Do We Store Your Data?
          </Heading>
          <Text>
            If you use the hosted version, your data is stored in our database
            (hosted in AWS). If you self-host, everything stays on your own
            machine.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            4. Your Rights
          </Heading>
          <Text>
            You can delete your data at any time. If you want your account
            removed, contact me and I’ll confirm everything is removed (there is
            a delete button on your account settings as well)
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            5. Security Measures
          </Heading>
          <Text>
            We have Row Level Security (RLS) enabled on all user data tables
            within the database.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            6. Questions?
          </Heading>
          <Text>
            If you have any concerns or questions, feel free to{" "}
            <a
              href="/contact-me"
              style={{ color: "blue", textDecoration: "underline" }}
            >
              contact me
            </a>
            .
          </Text>
        </Box>
      </VStack>
    </Box>
  )
}
