import { createFileRoute, Link } from "@tanstack/react-router";
import { Box, Heading, Text, VStack } from "@chakra-ui/react";

export const Route = createFileRoute("/_layout/faq")({
  component: FAQPage,
});

export function FAQPage() {
  return (
    <Box maxW="800px" mx="auto" py={10} px={6}>
      <Heading as="h1" mb={6}>
        Frequently Asked Questions
      </Heading>

      <VStack spaceY={6} align="start">
        <Box>
          <Heading as="h3" size="md">
            Is there a way to track net worth?
          </Heading>
          <Text>
            Not yet, but soon. With the integration of Plaid, we have started building net worth tracking.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            Do you offer an integration with bank accounts?
          </Heading>
          <Text>
            Yes, we integrate with Plaid. This should allow most major (and even minor) banks to be connected.
            Let me know if you don't see your bank as an option. 
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            Is it safe to link my accounts?
          </Heading>
          <Text>
            Yes, we use Plaid's secure authentication process to link your accounts. We cannot generate transfers from your accounts -- just read transactions from them.
          </Text>
        </Box>
    <Box>
            <Heading as="h3" size="md">
              Can I use this without connecting my bank directly?
          </Heading>
          <Text>Yes, we allow direct upload of bank statements if you just want to track your expenses month over month.
            Some features will not be supported, such as net worth tracking, but the transaction visualization, reports and budgeting will still work.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            Is the app open source?
          </Heading>
          <Text>
            Yes, the app is open source (
                      <Link to="https://github.com/mmc102/yearly_report" target="_blank" style={{ textDecoration: "underline", color: "blue" }}>
              here
            </Link>
            ). If you want to self-host, you're welcome to do that.
          </Text>
        </Box>

        
      </VStack>
    </Box>
  );
}
