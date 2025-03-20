import { Flex } from "@chakra-ui/react";

import { createFileRoute, redirect } from "@tanstack/react-router";
import { DemoTransactions } from "./_layout/transactions";
import { SegmentedNavigation } from "@/components/Common/SegmentedNavigation";

export const Route = createFileRoute("/demo")({
  component: DemoLayout,
  beforeLoad: async () => {
    throw redirect({
      to: "/landing",
    });
  },
});

export default function DemoLayout() {
  return (
    <div style={{ backgroundColor: "background", minHeight: "100vh" }}>
      <SegmentedNavigation />
      <Flex justify="center" align="center" width="full">
        <DemoTransactions />
      </Flex>
    </div>
  );
}
