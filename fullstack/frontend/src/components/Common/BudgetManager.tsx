import {
  type BudgetCategoryLinkOut,
  type BudgetEntryOut,
  BudgetsService,
} from "@/client"
import { DeleteIcon, EditIcon } from "@chakra-ui/icons"
import {
  Box,
  Button,
  HStack,
  Input,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRoot,
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
import type { BudgetOut, BudgetStatus, BudgetEntryStatus } from "../../client"
import EditBudgetEntry from "./EditBudgetEntry"

export const ManageBudget = ({ budget, budgetStatus }: { budget: BudgetOut, budgetStatus: BudgetStatus }) => {
  const queryClient = useQueryClient()

  const budgetEntryLookup: Record<BudgetEntryOut["id"], BudgetEntryOut> = budget.entries.reduce( (acc, entry) => {
    acc[entry.id] = entry
    return acc
  }, {})

  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] })
    },
  })

  return (
    <Box>
      <VStack gap={4}>
        <TableRoot variant="outline">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Budget Entry</TableColumnHeader>
              <TableColumnHeader>Amount Per Month</TableColumnHeader>
              <TableColumnHeader>Categories</TableColumnHeader>
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {budgetStatus.entry_status?.sort().map((entry,index) => (
              <>
              <TableRow key={index}>
                <TableCell minW={60}>{entry.name}</TableCell>
                <TableCell>{entry.amount}</TableCell>
                <TableCell>
                  {entry.category_links_status?.map((category) => (
                    <CategoryLink key={category.id} category={category} />
                  ))}
                </TableCell>
                <TableCell textAlign="right">
                  <ActionsCell
                    entry={budgetEntryLookup[entry.id]}
                    deleteEntryMutation={deleteEntryMutation}
                  />
                </TableCell>
                </TableRow>
                <TableRow>
                <CategoryLevelTable budgetEntryStatus={entry}/>
                </TableRow>
                </>
            ))}
          </TableBody>
        </TableRoot>
        <CreateNew budgetId={budget.id} />
      </VStack>
    </Box>
  )
}

function CategoryLevelTable({ budgetEntryStatus }: { budgetEntryStatus: BudgetEntryStatus }) {

  return (
    <TableRoot>
      <TableHeader>
        <TableRow>
          <TableColumnHeader>Category</TableColumnHeader>
          <TableColumnHeader>Total</TableColumnHeader>
        </TableRow>
      </TableHeader>
      <TableBody>
        {budgetEntryStatus.category_links_status.map( (entry)=>(
          <TableRow>
            <TableCell>{entry.stylized_name}</TableCell>
            <TableCell>{68}</TableCell>
          </TableRow>
        )
        )}
      </TableBody>
    </TableRoot>
  )
}

function CreateNew({ budgetId }: { budgetId: number }) {
  const [newEntry, setNewEntry] = useState<string>("")
  const queryClient = useQueryClient()

  const addEntryMutation = useMutation({
    mutationFn: (amount: number) =>
      BudgetsService.createBudgetEntry({
        budgetId,
        requestBody: { name: newEntry, amount: amount },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] })
      setNewEntry("")
    },
  })

  return (
    <HStack w="full">
      <Input
        placeholder="New entry name"
        maxW={200}
        value={newEntry}
        onChange={(e) => setNewEntry(e.target.value)}
      />
      <Button
        size="sm"
        onClick={() => addEntryMutation.mutate(0)}
        disabled={!newEntry.trim()}
      >
        <FaPlus />
        Add Entry
      </Button>
    </HStack>
  )
}

function ActionsCell({
  entry,
  deleteEntryMutation,
}: {
  entry: BudgetEntryOut
  deleteEntryMutation: UseMutationResult<unknown, Error, number, unknown>
}) {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <HStack>
      <Button size="sm" aria-label="Edit" onClick={() => setIsOpen(true)}>
        <EditIcon /> Edit
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
        Delete
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
