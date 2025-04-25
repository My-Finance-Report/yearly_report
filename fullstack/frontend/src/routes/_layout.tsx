import { Footer } from "@/components/Common/Footer";
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation";
import { Box, Flex } from "@chakra-ui/react";
import { Outlet, createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/_layout")({
  component: Layout,
});

function Layout() {
  return (
    <Box backgroundColor="background" minHeight="100vh">
      <Flex
        direction="column"
        justifyContent="space-between"
        backgroundColor="background"
      >
        <SegmentedNavigation />
        <Box minHeight="100vh">
          <Outlet />
        </Box>
        <Footer />
      </Flex>
    </Box>
  );
}
