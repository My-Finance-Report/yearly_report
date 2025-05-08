import { createFileRoute } from "@tanstack/react-router";
import { NoCodePage } from "@/components/NoCode/Pages";
import { z } from "zod";

const accountsSearchSchema = z.object({
  edit: z.boolean().optional(),
});

export const Route = createFileRoute("/_layout/_logged_in/accounts")({
  validateSearch: (search: Record<string, unknown>) =>
    accountsSearchSchema.parse(search),
  component: () => <NoCodePage variant={"account-page"} />,
});
