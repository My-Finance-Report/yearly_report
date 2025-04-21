import { createFileRoute } from '@tanstack/react-router'
import { Box, Heading, Link, Text, VStack } from "@chakra-ui/react"

export const Route = createFileRoute('/_layout/faq')({
  component: RouteComponent,
})

function RouteComponent() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6}>
      <Heading as="h1" mb={6}>
        FAQ
      </Heading>

      <VStack spaceY={6} align="start">
        <Box>
          <Heading as="h3" size="md">
            Need Help? Let's Chat!
          </Heading>
          <Text>
            If you're confused at any point, just{" "}
            <Link
              href="/contact-me"
              color="blue.500"
              textDecoration="underline"
            >
             let me know 
            </Link>{" "}
          and I can try to help.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            I linked my accounts but nothing has happened yet?
          </Heading>
          <Text mb={2}>
            Sometimes, Plaid has latency between linking and the trasnactions being available in the API. 
            This is often institution specific. (For example Vanguard seems to take a long time.)
            We have a background tasks that checks for new transactions every minute, so normally as soon 
            as they become available we will show them. 

          </Text>
          <Text>
            In short order I will add email notification, so you can be pinged when the transactions are ingested. 
          </Text>
        </Box>
        <Box>
          <Heading as="h3" size="md">
            How do I use the filters on the dashboard?
          </Heading>
          <Text mb={2}>
            I have preloaded a few useful filters, which can be selected from the dropdown menu. These will alter both
            the visuals, as well as the contents of the table below. To alter the filters, you can click the "show advanced filtering options" button. 
          </Text>
          <Text mb={2}>
            From here you will see stacked cards that say things like "Category", "Account", "Year", "Month" etc.
          </Text>
          <Text mb={2}>
            Each card represents a way to group and filter transations. 
          </Text>
          <Text mb={2}>
            So Category stacked on top of Month will group transactions by category and then by month. 
          </Text>
          <Text mb={2}>
            If you only care about the category of "Groceries", you can click the small filter icon on the card and select just that group.
          </Text>
          <Text mb={2}>
            To maintain the filtering, but prevent it from creating a visual grouping in the table -- for example to just show transactions from 2025, 
            you can click the filter, select 2025, then toggle the eye icon. Now we will only display transactions from 2025, but there will not be a visual grouping in the table. 
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            How to add a new category? 
          </Heading>
          <Text mb={2}>
           Head to Manage Accounts in the nav bar and select the account. From here you can add a new category, or rename an existing one. 
          </Text>
          <Text mb={2}>
           There is a "Recategorize" button that will allow you to re-categorize transactions for that account. 
          </Text>
        </Box>
      </VStack>
    </Box>
  )
}
