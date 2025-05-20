import { createFileRoute } from "@tanstack/react-router";
import { Box, Flex, Table, Text, Button, Breadcrumb } from "@chakra-ui/react";
import { useState } from "react";
import { OrderBase_Output, PosService } from "@/client";
import { useQuery } from "@tanstack/react-query";

export const Route = createFileRoute("/_layout/_logged_in/sales-reports")({
  component: RouteComponent,
});

interface Action {
  name: string;
  timeframe: "today" | "week" | "month" | "year" | "all";
}

const actions: Action[] = [
  { name: "Today", timeframe: "today" },
  { name: "This Week", timeframe: "week" },
  { name: "This Month", timeframe: "month" },
  { name: "This Year", timeframe: "year" },
  { name: "All Time", timeframe: "all" },
];

function AllActions({
  setTimeframe,
}: {
  setTimeframe: (timeframe: Action["timeframe"]) => void;
}) {
  return (
    <Flex direction={"column"} gap={2}>
      {actions.map((action, index) => (
        <ActionCard key={index} action={action} setTimeframe={setTimeframe} />
      ))}
    </Flex>
  );
}
function ActionCard({
  action,
  setTimeframe,
}: {
  action: Action;
  setTimeframe: (timeframe: Action["timeframe"]) => void;
}) {
  return (
    <Box
      cursor="pointer"
      minH={100}
      onClick={() => setTimeframe(action.timeframe)}
      display="flex"
      flexDirection="column"
      justifyContent="center"
      alignItems="center"
      p={4}
      minW={200}
      border="1px solid #ccc"
      borderRadius={4}
      _hover={{ backgroundColor: "gray.50" }}
    >
      <Text fontSize="lg" fontWeight="medium">
        {action.name}
      </Text>
    </Box>
  );
}

function determinePrice(item: OrderBase_Output["orderItems"][0]) {
  const variantPriceDelta = item.variants.reduce(
    (sum, variant) => sum + Number(variant.priceDelta),
    0,
  );
  return (Number(item.orderable.price) + variantPriceDelta) * item.quantity;
}

function formatPrice(price: number) {
  return price.toFixed(2);
}

function DataDisplay({ orders }: { orders: OrderBase_Output[] }) {
  return (
    <Table.Root>
      <Table.Header>
        <Table.Row>
          <Table.ColumnHeader>Date</Table.ColumnHeader>
          <Table.ColumnHeader>Total</Table.ColumnHeader>
          <Table.ColumnHeader>Actions</Table.ColumnHeader>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        {orders.map((order) => (
          <Table.Row key={order.id}>
            <Table.Cell>
              {new Date(order.timestamp).toLocaleDateString()}
            </Table.Cell>
            <Table.Cell>
              $
              {formatPrice(
                order.orderItems.reduce(
                  (sum, item) => sum + determinePrice(item),
                  0,
                ),
              )}
            </Table.Cell>
            <Table.Cell>
              <Button size="sm">View</Button>
            </Table.Cell>
          </Table.Row>
        ))}
      </Table.Body>
    </Table.Root>
  );
}

function RouteComponent() {
  const [timeframe, setTimeframe] = useState<Action["timeframe"]>("today");

  const { data: orders } = useQuery({
    queryKey: ["orders", timeframe],
    queryFn: () => PosService.getOrders(),
  });

  return (
    <>
      <BreadcrumbComponent />
      <Flex direction="column" gap={4}>
        <AllActions setTimeframe={setTimeframe} />
        {orders && <DataDisplay orders={orders} />}
      </Flex>
    </>
  );
}

function BreadcrumbComponent() {
  return (
    <Breadcrumb.Root size="lg">
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Sales Reports</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
