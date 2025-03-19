import { createFileRoute } from '@tanstack/react-router'
import { Box, Heading, Text, VStack } from '@chakra-ui/react'

export const Route = createFileRoute('/_layout/terms')({
  component: Terms
})

function Terms() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6}>
      <Heading as="h1" mb={6}>
        Terms of Service
      </Heading>

      <VStack spaceY={6} align="start">
        <Box>
          <Heading as="h3" size="md">
            1. Acceptance of Terms
          </Heading>
          <Text>
            By using this app, you agree to follow these Terms of Service. 
            If you don’t agree, please don’t use the app.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            2. What This App Does
          </Heading>
          <Text>
            This app helps you track your spending habits by allowing 
            you to upload bank statements and analyze cash flow. 
            It does not provide financial advice.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            3. Your Data
          </Heading>
          <Text>
            - We don’t sell your data.  
            - You can delete your data at any time.  
            - If you self-host, your data never leaves your server.  
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            4. Liability
          </Heading>
          <Text>
            We are not responsible for financial losses, incorrect 
            calculations, or bad budgeting decisions made using this tool.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            5. Changes to These Terms
          </Heading>
          <Text>
            We may update these terms from time to time. If you continue 
            using the app after changes, that means you accept them.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            6. Questions?
          </Heading>
          <Text>
            Reach out on the{' '}
            <a href="/contact-me" style={{ textDecoration: 'underline', color: 'blue' }}>
              contact page
            </a>{' '}
            if you have any questions.
          </Text>
        </Box>
      </VStack>
    </Box>
  )
}
