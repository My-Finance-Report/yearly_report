import React, { useRef } from "react";
import { createFileRoute, useNavigate } from "@tanstack/react-router";
import { DemoTransactions } from "./_layout/transactions";
import { Box, Flex, Button, Heading, Text, Image, Highlight } from "@chakra-ui/react";
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation";
import { useIsMobile } from "@/hooks/useIsMobile";

export const Route = createFileRoute("/landing")({
  component: Landing,
});

function Landing() {
  const isMobile = useIsMobile();
  const navigate = useNavigate();

  const demoSectionRef = useRef<HTMLDivElement>(null);

  const scrollToDemo = () => {
    demoSectionRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  return (
    <Flex direction="column" minHeight="100vh" backgroundColor="background" alignItems="center">
      <SegmentedNavigation />
      <Box as="main" maxW="1200px" mx="auto" px={4} py={12}>
        <Flex
          direction={{ base: "column", md: "row" }}
          align="center"
          justify="space-between"
          mb={{ base: 6, md: 12 }}
          gap={6}
        >
          <Box flex="1" marginRight={isMobile ? 0 : 24}>
            <Heading as="h1" size="5xl" mb={4}>
              <Highlight  query="finances" styles={{color: "#8b55e9"}}>
                Because you should love your finances
              </Highlight>
            </Heading>
            <Text fontSize="lg" maxW="600px" lineHeight="tall" >
              Love shouldn’t be complicated—neither should your finances. My
              Financé gives you a clear, visual breakdown of your spending each
              month.
            </Text>
            <Flex marginTop={8} gap={2}>
              <Button
                variant="solid"
                onClick={() => navigate({ to: "/signup" })}
              >
                Sign Up
              </Button>
              <Button
                variant="outline"
                onClick={scrollToDemo}
              >
                Try the Demo
              </Button>
            </Flex>
          </Box>
          <Box flex="1" textAlign="center">
            <Image
              src="/assets/images/landing.png"
              alt="My Financé illustration"
              borderRadius="md"
              mx="auto"
            />
          </Box>
        </Flex>

<Box mt={8}>
  <Flex
    direction={isMobile ? "column" : "row"}
    gap={10}
    alignItems="stretch" // Ensures all boxes stretch to the same height
  >
    {[ 
      { title: "Bank Statement Extraction", text: "Easily extract transactions from your bank account, no manual data entry required." },
      { title: "Clear & Visual Breakdown", text: "Automatically categorize your transactions and display clean visualizations." },
      { title: "Budgeting", text: "Set up a budget and get insights into your progress over months and years." }
    ].map(({ title, text }) => (
      <Box
        key={title}
        border="1px solid"
        borderRadius="md"
        p={6}
        flex="1" 
        minWidth="250px" 
        display="flex"
        flexDirection="column"
        justifyContent="space-between"
      >
        <Heading as="h3" size="md" mb={2}>
          {title}
        </Heading>
        <Text>{text}</Text>
      </Box>
    ))}
  </Flex>
</Box>

      </Box>
      <Box ref={demoSectionRef} mt={24} textAlign="center" mb={10}>
        <Heading as="h3" size="2xl" >
          Your Dashboard will look like this:
        </Heading>
      </Box>
      <Box  border="1px solid"  borderRadius="md" m={isMobile ? 1 : 6} p={isMobile ? 1 : 6} backgroundColor="background" >
      <DemoTransactions />
  </Box>
    </Flex>
  );
}

export default Landing;
