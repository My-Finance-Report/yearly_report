import React from "react";
import { BudgetEntryOut, BudgetsService } from "@/client";
import { CheckIcon, CloseIcon, DeleteIcon, EditIcon } from "@chakra-ui/icons";
import {
  Box,
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
  UseMutateFunction,
  useMutation,
  UseMutationResult,
  useQuery,
  useQueryClient,
} from "@tanstack/react-query";
import { useState } from "react";
import { FaPlus } from "react-icons/fa";
import { BudgetOut } from "../../client";
import EditBudgetEntry from "./EditBudgetEntry";

export const ManageBudget = ({ budget }: { budget: BudgetOut }) => {
  const queryClient = useQueryClient();
  const [newEntry, setNewEntry] = useState<string>("");
  const [editingEntry, setEditingEntry] = useState<BudgetEntryOut | null>(null);

  const addEntryMutation = useMutation({
    mutationFn: (amount: number) =>
      BudgetsService.createBudgetEntry({
        budgetId: budget.id,
        requestBody: { name: newEntry, amount: amount },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
      setNewEntry("");
    },
  });
  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
    },
  });
  const updateEntryMutation = useMutation({
    mutationFn: ({
      entryId,
      name,
      amount,
    }: {
      entryId: number;
      name: string;
      amount: string;
    }) =>
      BudgetsService.updateBudgetEntry({
        entryId: entryId,
        requestBody: {
          id: entryId,
          category_links: [],
          name,
          amount,
          budget_id: budget.id,
        },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
    },
  });

  console.log(editingEntry)

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
                <TableCell minW={60}>
                  {entry.name}
                </TableCell>
                <TableCell>
                  {entry.amount}
                </TableCell>
                <TableCell>
                  {entry.category_links?.map((category) => (
                    <Text key={category.id}>{category.stylized_name}</Text>
                  ))}
                </TableCell>
                <TableCell textAlign="right">
                  {editingEntry?.id === entry.id ? (
                    <HStack>
                      <Button
                        size="sm"
                        aria-label="Save"
                        onClick={() =>
                          updateEntryMutation.mutate({
                            entryId: entry.id,
                            name: editingEntry.name,
                            amount: editingEntry.amount,
                          })
                        }
                      >
                        {" "}
                        <CheckIcon />
                        Save
                      </Button>
                      <Button
                        size="sm"
                        aria-label="Cancel"
                        onClick={() => setEditingEntry(null)}
                      >
                        <CloseIcon />
                      </Button>
                    </HStack>
                  ) : (
                    <ActionsCell
                      setEditingEntry={setEditingEntry}
                      entry={entry}
                      deleteEntryMutation={deleteEntryMutation}
                    />
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </TableRoot>

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
          <EditBudgetEntry
            isOpen={editingEntry !== null}
            onClose={() => setEditingEntry(null)}
            budgetEntry={editingEntry!}
          />
        </HStack>
      </VStack>
    </Box>
  );
};

function ActionsCell({
  setEditingEntry,
  entry,
  deleteEntryMutation,
}: {
  setEditingEntry: (entry: BudgetEntryOut) => void;
  entry: BudgetEntryOut;
  deleteEntryMutation:UseMutationResult<unknown, Error, number, unknown> ;
}) {
  return (
    <HStack>
      <Button
        size="sm"
        aria-label="Edit"
        onClick={() => setEditingEntry(entry)}
      >
        <EditIcon /> Edit
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
