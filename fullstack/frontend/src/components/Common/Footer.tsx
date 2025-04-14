import { Box, Flex, Link, Text } from "@chakra-ui/react"

export function Footer() {
  return (
    <Box as="footer" position="sticky" bottom={0} w="100%">
      <Flex
        alignSelf={"flex-end"}
        mx="auto"
        justify="space-between"
        align="center"
        backgroundColor="background"
        flexWrap="wrap"
        p={2}
        mt={2}
      >
        <Text fontSize="lg" fontWeight="bold"></Text>

        <Flex gap={6} flexWrap="wrap">
          <Link href="/contact-me" _hover={{ textDecoration: "underline" }}>
            Contact Me
          </Link>
          <Link href="/how" _hover={{ textDecoration: "underline" }}>
            How Do I Use This?
          </Link>
          <Link href="/faq" _hover={{ textDecoration: "underline" }}>
            FAQ
          </Link>
          <Link href="/privacy" _hover={{ textDecoration: "underline" }}>
            Privacy Policy
          </Link>
          <Link href="/terms" _hover={{ textDecoration: "underline" }}>
            Terms of Service
          </Link>
        </Flex>
      </Flex>
    </Box>
  )
}
