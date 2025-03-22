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
            No, this tool mainly aims to capture the flow of money. 
            You should use it to determine how much you spend on groceries 
            each month, for example.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            Do you offer an integration with bank accounts?
          </Heading>
          <Text>
            Not directly. You can upload bank statements for now. If you 
            want an integration with Plaid, reach out to me{" "}
            <Link to="/contact" style={{ textDecoration: "underline", color: "blue" }}>
              here
            </Link>
            . I’ll build the integration if a few people ask.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            Why should I trust you?
          </Heading>
          <Text>
            You don’t have to. The app is open source (
                      <Link to="https://github.com/mmc102/yearly_report" target="_blank" style={{ textDecoration: "underline", color: "blue" }}>
              here
            </Link>
            ). If you want to self-host, you're welcome to do that.
          </Text>
        </Box>

        <Box>
          <Heading as="h3" size="md">
            How do you parse bank statements without configuration?
          </Heading>
          <Text>LLMs.</Text>
        </Box>
      </VStack>
    </Box>
  );
}
