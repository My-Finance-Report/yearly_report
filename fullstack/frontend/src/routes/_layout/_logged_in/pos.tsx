import { Box, Flex, Text } from "@chakra-ui/react";
import { createFileRoute, Link } from "@tanstack/react-router";

export const Route = createFileRoute("/_layout/_logged_in/pos")({
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
    link: "/order",
  },
  {
    name: "Sales Reports",
    link: "/sales-reports",
  },
  {
    name: "Manage Menu",
    link: "/manage-menu",
  },
];

function AllActions() {
  return (
    <Flex direction={"column"} gap={2}>
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
        minW={200}
        border="1px solid #ccc"
        borderRadius={4}
      >
        <Text cursor="pointer">{action.name}</Text>
      </Box>
    </Link>
  );
}
