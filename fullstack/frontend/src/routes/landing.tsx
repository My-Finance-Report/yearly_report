import React from "react"
import { createFileRoute, useNavigate } from "@tanstack/react-router"
import {
  Box,
  Flex,
  Button,
  Heading,
  Text,
  Image,
} from "@chakra-ui/react"
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"
import { useIsMobile } from "@/hooks/useIsMobile"

export const Route = createFileRoute("/landing")({
  component: Landing,
})

function Landing() {
    const isMobile = useIsMobile();
    const navigate = useNavigate();
  return (
    <div style={{backgroundColor: "background"}}>
    <SegmentedNavigation />
    <Box as="main" maxW="1200px" mx="auto" px={4} py={8}>
      <Flex
        direction={{ base: "column", md: "row" }}
        align="center"
        justify="space-between"
        mb={{ base: 6, md: 12 }}
        gap={6}
      >
        <Box flex="1">
          <Heading as="h1" size="xl" mb={4}>
            Because you should love your finances
          </Heading>
          <Text fontSize="lg" maxW="600px" lineHeight="tall">
            Love shouldn’t be complicated—neither should your finances.
            My Financé gives you a clear, visual breakdown of your spending each month.
          </Text>
          <Flex marginTop={4} gap={2}>
          <Button variant="outline" onClick={() => navigate({ to: "/demo" })}>
            Try the Demo
          </Button>
          <Button variant="solid" onClick={() => navigate({ to: "/signup" })}>
            Sign Up
          </Button>
          </Flex>
          <Text fontSize="sm" fontWeight="bold" mt={2}>$20/year</Text>
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

      <Flex direction={isMobile ? "column" : "row"} gap={2}>
          <Box
            border="1px solid"
            borderColor="gray.200"
            borderRadius="md"
            p={6}
          >
            <Heading as="h3" size="md" mb={2}>
              1. Bank Statement Extraction
            </Heading>
            <Text>
              Automatically extract transactions from your bank statements,
              no manual data entry required.
            </Text>
          </Box>

          <Box
            border="1px solid"
            borderColor="gray.200"
            borderRadius="md"
            p={6}
          >
            <Heading as="h3" size="md" mb={2}>
              2. Clear &amp; Visual Breakdown
            </Heading>
            <Text>
              Our visual analytics break down your spending by category,
              giving you a simple, high-level overview of your finances.
            </Text>
          </Box>

          <Box
            border="1px solid"
            borderColor="gray.200"
            borderRadius="md"
            p={6}
          >
            <Heading as="h3" size="md" mb={2}>
              3. (Optional) Budgeting
            </Heading>
            <Text>
                Setup your budget to track your spending and build better habits.
            </Text>
          </Box>
        </Flex>
      </Box>
    </Box>
</div>
  )
}

export default Landing
