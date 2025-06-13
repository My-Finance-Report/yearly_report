import { Box, Flex, Link, Text } from "@chakra-ui/react";
import { getCurrentUser } from "@/hooks/useAuth";
import { useQuery } from "@tanstack/react-query";
import { useIsMobile } from "@/hooks/useIsMobile";

export function Footer() {
  const { data: currentUser } = useQuery({
    queryKey: ["currentUser"],
    queryFn: getCurrentUser,
    retry: false,
  });

  const isMobile = useIsMobile();

  if (currentUser?.settings?.point_of_sales_user) {
    return null;
  }

  if (isMobile) {
    return null;
  }

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
  );
}
