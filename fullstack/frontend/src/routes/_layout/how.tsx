import { createFileRoute } from '@tanstack/react-router'
import { Box, Heading, Text, VStack, Button, Link } from '@chakra-ui/react'

export const Route = createFileRoute('/_layout/how')({
    component: HowPage
})

export function HowPage() {
    return (
        <Box maxW="800px" mx="auto" py={10} px={6}>
            <Heading as="h1" mb={6}>
                How It Works
            </Heading>

            <VStack spaceY={6} align="start">
                <Box>
                    <Heading as="h3" size="md">
                        Need Help? Let's Chat!
                    </Heading>
                    <Text>
                        If you're confused at any point, you can{' '}
                        <Link href="/contact-me" color="blue.500" textDecoration="underline">
                            schedule a call with me
                        </Link>{' '}
                        and I’ll walk you through it.
                    </Text>
                </Box>

                <Box>
                    <Heading as="h3" size="md">
                        1. Upload Your Bank Statements
                    </Heading>
                    <Text>
                        Head over to the{' '}
                        <Link href="/upload-files" color="blue.500" textDecoration="underline">
                            Upload Page
                        </Link>{' '}
                        and drop in your bank statements. The app will automatically
                        process them for analysis. (it may take a few minutes for them to process, consider refreshing the page)
                    </Text>
                </Box>

                <Box>
                    <Heading as="h3" size="md">
                        2. View Your Data in the Dashboard
                    </Heading>
                    <Text>
                        Once uploaded, navigate to your{' '}
                        <Link href="/transactions" color="blue.500" textDecoration="underline">
                            Dashboard
                        </Link>{' '}
                        to see insights into your spending habits, categorized transactions,
                        and trends over time.
                    </Text>
                </Box>

                <Box>
                    <Heading as="h3" size="md">
                        3. Track Your Cash Flow
                    </Heading>
                    <Text>
                        Use the dashboard to understand where your money is going
                        and make informed financial decisions.
                    </Text>
                </Box>

                <Box>
                    <Heading as="h3" size="md" mb={2}>
                        Still Have Questions?
                    </Heading>

                    <Link href="/contact-me">
                        <Button>
                            I will help you directly!
                        </Button>
                    </Link>
                </Box>
            </VStack>
        </Box>
    )
}
