import { Box, Flex, Text } from "@chakra-ui/react";
import { createFileRoute, Link } from "@tanstack/react-router";

export const Route = createFileRoute("/_layout/_logged_in/pos/pos")({
  component: RouteComponent,
});

function RouteComponent() {
  return <AllActions />;
}

interface Action {
  name: string;
  link: string;
}

const actions: Action[] = [
  {
    name: "New Order",
    link: "/pos/order",
  },
  {
    name: "Recent Orders",
    link: "/pos/recent-orders",
  },
  {
    name: "Sales Reports",
    link: "/pos/sales-reports",
  },
  {
    name: "Manage Menu",
    link: "/pos/manage-menu",
  },
];

function AllActions() {
  return (
    <Flex direction={"column"} gap={10} p={10}>
      {actions.map((action, index) => (
        <ActionCard key={index} action={action} />
      ))}
    </Flex>
  );
}
function ActionCard({ action }: { action: Action }) {
  return (
    <Link to={action.link}>
      <Box
        cursor="pointer"
        minH={100}
        onClick={() => console.log(action)}
        display={"flex"}
        flexDirection={"column"}
        justifyContent="center"
        alignItems="center"
        p={2}
        minW="90%"
        border="1px solid #ccc"
        borderRadius={4}
      >
        <Text cursor="pointer">{action.name}</Text>
      </Box>
    </Link>
  );
}
