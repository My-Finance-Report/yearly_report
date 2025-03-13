import React from "react"
import { createFileRoute } from "@tanstack/react-router"
import {
  Box,
  Flex,
  Button,
  Heading,
  Text,
  Image,
  SimpleGrid,
} from "@chakra-ui/react"
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation"

export const Route = createFileRoute("/landing")({
  component: Landing,
})

function Landing() {
  return (
    <>
    <SegmentedNavigation />
    <Box as="main" maxW="1200px" mx="auto" px={4} py={8}>
      {/* Hero Section */}
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
            My Financé gives you a clear, visual breakdown of your spending,
            so you can take control of your budget and build a future you love.
          </Text>
          <Flex marginTop={4} gap={2}>
          <Button variant="outline">
            Try the Demo
          </Button>
          <Button variant="solid">
            Sign Up
          </Button>
          </Flex>
        </Box>
        <Box flex="1" textAlign="center">
          {/* Replace with your actual hero image source */}
          <Image
            src="/assets/images/landing.png"
            alt="My Financé illustration"
            borderRadius="md"
            mx="auto"
          />
        </Box>
      </Flex>

      {/* Directions / Cards Section */}
      <Box mt={8}>
        <Heading as="h2" size="lg" mb={6}>
          Love takes patience, this doesn’t
        </Heading>

        <SimpleGrid columns={{ base: 1, md: 3 }} spaceX={6}>
          {/* Card 1 */}
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

          {/* Card 2 */}
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

          {/* Card 3 */}
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
        </SimpleGrid>
      </Box>
    </Box>
</>
  )
}

export default Landing
