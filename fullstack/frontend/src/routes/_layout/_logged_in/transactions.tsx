import { createFileRoute } from "@tanstack/react-router";
import { FilterProvider } from "@/contexts/FilterContext";
import { z } from "zod";
import { TransactionsService } from "@/client";
import { TransactionsView } from "@/components/Common/Transactions/TransactionsView";
import { WorkerStatus } from "@/components/Common/WorkerStatus";
import { NullState } from "@/components/Common/LandingPageNullState";
import { Spinner } from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";

const transactionsSearchSchema = z.object({
  filter: z.string().optional(),
});

export const Route = createFileRoute("/_layout/_logged_in/transactions")({
  component: Transactions,
  validateSearch: (search) => transactionsSearchSchema.parse(search),
});

export function Transactions({
  isDemo=false,
}: {
  isDemo: boolean;
}) {
  const { data: state } = useQuery({
    queryKey: ["checkStatusOfLanding"],
    queryFn: TransactionsService.getLandingStatus,
  });

  switch (state) {
    case "has_transactions":
      return (
        <FilterProvider isDemo={isDemo}>
          <TransactionsView isDemo={isDemo} />
        </FilterProvider>
      );
    case "no_transactions_not_processing":
      return <NullState />;
    case "no_transactions_processing":
      return <WorkerStatus />;
  }
  return <Spinner />;
}



