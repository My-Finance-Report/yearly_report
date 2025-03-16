import {
  type BudgetCategoryLinkOut,
  type BudgetEntryOut,
  BudgetsService,
} from "@/client"
import { DeleteIcon, EditIcon } from "@chakra-ui/icons"
import {
  Button,
  HStack,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  Table,
  TableRow,
  Tag,
  Text,
  VStack,
} from "@chakra-ui/react"
import {
  type UseMutationResult,
  useMutation,
  useQueryClient,
} from "@tanstack/react-query"
import { useState } from "react"
import { FaPlus } from "react-icons/fa"
import type { BudgetOut, BudgetStatus} from "../../client"
import EditBudgetEntry from "./EditBudgetEntry"
import { formatCurrency } from '../Charting/PieChart'
import { useIsMobile } from "@/hooks/useIsMobile"

export const ManageBudget = ({ budget, budgetStatus }: { budget: BudgetOut, budgetStatus: BudgetStatus }) => {
  const queryClient = useQueryClient()

  const budgetEntryLookup: Record<BudgetEntryOut["id"], BudgetEntryOut> = budget.entries.reduce((acc, entry) => {
    acc[entry.id] = entry
    return acc
  }, {} as Record<BudgetEntryOut["id"], BudgetEntryOut>)

  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgetStatus"] })
    },
  })

  const isMobile = useIsMobile()

  return (

    <VStack>
        <CreateNew budgetId={budget.id} />
        <Table.Root variant="outline" borderRadius="md">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Budget Entry</TableColumnHeader>
              <TableColumnHeader>Target <Text fontSize="xs">(/month)</Text></TableColumnHeader>
              {!isMobile && (
                <TableColumnHeader>Categories</TableColumnHeader>
              )}
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {budgetStatus.entry_status?.sort().map((entry, index) => (
              <>
                <TableRow key={index}>
                  <TableCell>{entry.name}</TableCell>
                  <TableCell>{formatCurrency(Number(entry.amount))}</TableCell>
                  {!isMobile &&
                  <TableCell>
                    {budgetEntryLookup[entry.id]?.category_links?.map((category) => (
                      <CategoryLink key={category.id} category={category} />
                    ))}
                  </TableCell>
}
                  <TableCell textAlign="right">
                    <ActionsCell
                      entry={budgetEntryLookup[entry.id]}
                      isMobile={isMobile}
                      deleteEntryMutation={deleteEntryMutation}
                    />
                  </TableCell>
                </TableRow>
              </>
            ))}
          </TableBody>
        </Table.Root>
  </VStack>
  )
}



function CreateNew({ budgetId }: { budgetId: number }) {
  const [newEntry, setNewEntry] = useState<BudgetEntryOut | null>(null)
  const [isOpen, setIsOpen] = useState(false)
  const queryClient = useQueryClient()

  const addEntryMutation = useMutation({
    mutationFn: () =>
      BudgetsService.createBudgetEntry({
        budgetId,
        requestBody: { name: "", amount: 100 },
      }),
    onSuccess: (data: BudgetEntryOut) => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] })
      queryClient.invalidateQueries({ queryKey: ["budgetStatus"] })
      setNewEntry(data)
      setIsOpen(true)
    },
  })

  const handleCreate = () => {
    addEntryMutation.mutate()
  }

  return (
    <HStack w="full">
      <Button size="sm" onClick={handleCreate}>
        <FaPlus />
        Add Entry
      </Button>

      {newEntry && (
        <EditBudgetEntry
          isOpen={isOpen}
          onClose={() => {setIsOpen(false); setNewEntry(null)}}
          budgetEntry={newEntry}
        />
      )}
    </HStack>
  )
}


function ActionsCell({
  entry,
  deleteEntryMutation,
  isMobile,
}: {
  entry: BudgetEntryOut
  deleteEntryMutation: UseMutationResult<unknown, Error, number, unknown>
  isMobile: boolean
}) {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <HStack>
      <Button size="sm" aria-label="Edit" onClick={() => setIsOpen(true)}>
        <EditIcon /> {!isMobile && "Edit"}
        <EditBudgetEntry
          isOpen={isOpen}
          onClose={() => setIsOpen(false)}
          budgetEntry={entry}
        />
      </Button>
      <Button
        size="sm"
        colorScheme="red"
        aria-label="Delete"
        onClick={() => deleteEntryMutation.mutate(entry.id)}
      >
        <DeleteIcon />
        {!isMobile && "Delete"}
      </Button>
    </HStack>
  )
}

function CategoryLink({ category }: { category: BudgetCategoryLinkOut }) {
  return (
    <Tag.Root size="sm" m={2}>
      <Text key={category.id}>{category.stylized_name}</Text>
    </Tag.Root>
  )
}

