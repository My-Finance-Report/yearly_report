import { PosService } from "@/client";
import { createFileRoute } from "@tanstack/react-router";
import { useQuery } from "@tanstack/react-query";
import { OrderCard } from "@/components/Pos/Order";
import { Box, Breadcrumb, Flex, Heading, Text } from "@chakra-ui/react";

export const Route = createFileRoute("/_layout/_logged_in/pos/recent-orders")({
  component: RouteComponent,
});

function RouteComponent() {
  const { data } = useQuery({
    queryKey: ["orders"],
    queryFn: () => PosService.getOrders({ days: 2 }),
  });

  return (
    <Box p={4}>
      <BreadcrumbComponent />
      <Heading size="lg">Recent Orders</Heading>
      <Flex direction="column" gap={4}>
        {data?.map((order) => (
          <Box key={order.id} borderRadius={4} p={4} border="1px solid #ccc">
            <DatetimeForCard timestamp={order.timestamp} />
            <OrderCard order={order} setOrder={() => {}} allowEdits={false} />
          </Box>
        ))}
      </Flex>
    </Box>
  );
}

function DatetimeForCard({ timestamp }: { timestamp: string }) {
  const date = new Date(timestamp);
  let toShow = date.toLocaleDateString();

  if (date.toLocaleDateString() === new Date().toLocaleDateString()) {
    toShow = date.toLocaleTimeString([], {
      hour: "numeric",
      minute: "2-digit",
    });
  }

  return (
    <Text fontSize="xs" fontWeight="bold">
      {toShow}
    </Text>
  );
}

function BreadcrumbComponent() {
  return (
    <Breadcrumb.Root size="lg">
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Recent Orders</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
