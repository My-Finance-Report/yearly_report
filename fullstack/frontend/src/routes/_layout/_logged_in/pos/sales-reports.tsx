import { createFileRoute } from "@tanstack/react-router";
import { Box, Flex, Table, Text, Button, Breadcrumb, Collapsible } from "@chakra-ui/react";
import { useState } from "react";
import { OrderBase_Output, PosService } from "@/client";
import { useQuery } from "@tanstack/react-query";
import { OrderCard } from "@/components/Pos/Order";

export const Route = createFileRoute("/_layout/_logged_in/pos/sales-reports")({
  component: RouteComponent,
});

interface Action {
  name: string;
  timeframe?: number;
}

const actions: Action[] = [
  { name: "Today", timeframe: 1 },
  { name: "This Week", timeframe: 7 },
  { name: "This Month", timeframe: 30 },
  { name: "This Year", timeframe: 365 },
  { name: "All Time", timeframe: undefined },
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
    (sum, variant) => sum + Number(variant.price_delta),
    0,
  );
  return (Number(item.orderable.price) + variantPriceDelta) * item.quantity;
}

function formatPrice(price: number) {
  return price.toFixed(2);
}

function DataDisplay({ orders }: { orders: OrderBase_Output[] }) {
  const [isOpen, setIsOpen] = useState(new Map<string, boolean>());

  const fullTotal = orders.reduce((sum, order) => {
    return sum + order.orderItems.reduce((orderSum, item) => orderSum + determinePrice(item), 0);
  }, 0);

  return (
    <>
      <Table.Root>
        <Table.Header>
          <Table.Row>
            <Table.ColumnHeader>Date</Table.ColumnHeader>
            <Table.ColumnHeader>Order Total</Table.ColumnHeader>
            <Table.ColumnHeader>Actions</Table.ColumnHeader>
          </Table.Row>
        </Table.Header>
        <Table.Body>
          {orders.map((order) => (
            <>
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
                  <Button size="sm" onClick={() => setIsOpen((prev) => {
                    const newMap = new Map(prev);
                    newMap.set(order.id, !newMap.get(order.id));
                    return newMap;
                  })}>{isOpen.get(order.id) ? "Hide" : "View"}</Button>
                </Table.Cell>
              </Table.Row>
              <Table.Row >
                <Table.Cell colSpan={3} padding={isOpen.get(order.id) ? "4" : "0"}>
                  <Collapsible.Root open={isOpen.get(order.id) || false}>
                    <Collapsible.Content>
                      <OrderCard
                        setOrder={() => { }}
                        order={order}
                        allowEdits={false}
                      />
                    </Collapsible.Content>
                  </Collapsible.Root>
                </Table.Cell>
              </Table.Row>
            </>
          ))}
          <Table.Row>
            <Table.Cell colSpan={3}>
              <Text>Total: ${fullTotal.toFixed(2)}</Text>
            </Table.Cell>
          </Table.Row>
        </Table.Body>
      </Table.Root>
    </>
  );
}

function RouteComponent() {
  const [timeframe, setTimeframe] = useState<Action["timeframe"]>(1);

  const { data: orders } = useQuery({
    queryKey: ["orders", timeframe],
    queryFn: () => PosService.getOrders({days: timeframe}),
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
          <Breadcrumb.Link href="/pos/pos/">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Sales Reports</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
