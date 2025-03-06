import { BudgetCategoryLinkOut, BudgetEntryOut, BudgetsService, CategoryOut } from "@/client";
import { DeleteIcon, EditIcon } from "@chakra-ui/icons";
import {
  Box,
  Tag,
  Button,
  HStack,
  Input,
  Text,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRoot,
  TableRow,
  VStack,
} from "@chakra-ui/react";
import {
  useMutation,
  UseMutationResult,
  useQueryClient,
} from "@tanstack/react-query";
import { useState } from "react";
import { FaPlus } from "react-icons/fa";
import { BudgetOut } from "../../client";
import EditBudgetEntry from "./EditBudgetEntry";

export const ManageBudget = ({ budget }: { budget: BudgetOut }) => {
  const queryClient = useQueryClient();

  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
    },
  });

  return (
    <Box>
      <VStack gap={4}>
        <TableRoot variant="outline">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Budget Entry</TableColumnHeader>
              <TableColumnHeader>Amount</TableColumnHeader>
              <TableColumnHeader>Categories</TableColumnHeader>
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {budget.entries?.sort().map((entry) => (
              <TableRow key={entry.id}>
                <TableCell minW={60}>{entry.name}</TableCell>
                <TableCell>{entry.amount}</TableCell>
                <TableCell>
                  {entry.category_links?.map((category) => (
                    <CategoryLink key={category.id} category={category} />
                  ))}
                </TableCell>
                <TableCell textAlign="right">
                  <ActionsCell
                    entry={entry}
                    deleteEntryMutation={deleteEntryMutation}
                  />
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </TableRoot>
        <CreateNew budgetId={budget.id} />
      </VStack>
    </Box>
  );
};

function CreateNew({ budgetId }: { budgetId: number }) {
  const [newEntry, setNewEntry] = useState<string>("");
  const queryClient = useQueryClient();

  const addEntryMutation = useMutation({
    mutationFn: (amount: number) =>
      BudgetsService.createBudgetEntry({
        budgetId,
        requestBody: { name: newEntry, amount: amount },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
      setNewEntry("");
    },
  });

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
  );
}

function ActionsCell({
  entry,
  deleteEntryMutation,
}: {
  entry: BudgetEntryOut;
  deleteEntryMutation: UseMutationResult<unknown, Error, number, unknown>;
}) {
  const [isOpen, setIsOpen] = useState(false);
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
  );
}


function CategoryLink({category}: {category: BudgetCategoryLinkOut}) {
   return ( 
    <Tag.Root size="sm" m={2} >
    <Text key={category.id}>{category.stylized_name}</Text>
    </Tag.Root>
)
}