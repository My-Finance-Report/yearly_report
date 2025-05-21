import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/_layout/_logged_in/pos/manage-variants")(
  {
    component: RouteComponent,
  },
);

function RouteComponent() {
  return <div>Hello "/_layout/_logged_in/pos/manage-variants"!</div>;
}
