import { createFileRoute } from "@tanstack/react-router";
import { NoCodePage } from "@/components/NoCode/Pages";

export const Route = createFileRoute("/_layout/_logged_in/accounts")({
  component: () => <NoCodePage variant={"account-page"} />,
});
