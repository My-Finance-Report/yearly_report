import { TransactionsService } from "@/client"
import { NullState } from "@/components/Common/LandingPageNullState"
import { PageSpinner } from "@/components/Common/PageSpinner"
import { TransactionsView } from "@/components/Common/Transactions/TransactionsView"
import { WorkerStatus } from "@/components/Common/WorkerStatus"
import { FilterProvider } from "@/contexts/FilterContext"
import { useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { z } from "zod"

const transactionsSearchSchema = z.object({
  filter: z.string().optional(),
  showAdvanced: z.boolean().optional(),
})

export const Route = createFileRoute("/_layout/_logged_in/transactions")({
  component: Transactions,
  validateSearch: (search) => transactionsSearchSchema.parse(search),
})

export function Transactions({
  isDemo = false,
}: {
  isDemo: boolean
}) {
  const { data: state } = useQuery({
    queryKey: ["checkStatusOfLanding"],
    queryFn: TransactionsService.getLandingStatus,
    enabled: !isDemo,
  })

  if (isDemo) {
    return (
      <FilterProvider isDemo={isDemo}>
        <TransactionsView isDemo={isDemo} />
      </FilterProvider>
    )
  }

  switch (state) {
    case "has_transactions":
      return (
        <FilterProvider isDemo={isDemo}>
          <TransactionsView isDemo={isDemo} />
        </FilterProvider>
      )
    case "no_transactions_not_processing":
      return <NullState />
    case "no_transactions_processing":
      return <WorkerStatus />
  }
  return <PageSpinner />
}
