import React from "react";
import { BudgetsService } from "@/client";
import { CheckIcon, CloseIcon, DeleteIcon, EditIcon } from "@chakra-ui/icons";
import {
  Box,
  Button,
  HStack,
  Heading,
  Input,
  Spinner,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRoot,
  TableRow,
  VStack,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { useState } from "react";
import { FaPlus } from "react-icons/fa";
import { BudgetOut, BudgetEntryBase } from "../../client";

export const ManageBudget = ({ budget }: { budget: BudgetOut }) => {
  const queryClient = useQueryClient();
  const [newEntry, setNewEntry] = useState<string>("");
  const [editingEntry, setEditingEntry] = useState<BudgetEntryBase | null>(
    null
  );

  const addEntryMutation = useMutation({
    mutationFn: (amount: number) =>
      BudgetsService.createBudgetEntry({
        budgetId: budget.id,
        requestBody: { name: newEntry, amount: amount, budget_id: budget.id },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets", budget.id] });
      setNewEntry("");
    },
  });
  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets", budget.id] });
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
      amount: number;
    }) =>
      BudgetsService.updateBudgetEntry({
        entryId: entryId,
        requestBody: { name, amount, budget_id: budget.id },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets", budget.id] });
    },
  });

  return (
    <Box>
      <VStack gap={4}>
        <TableRoot variant="outline">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Category</TableColumnHeader>
              <TableColumnHeader>Actions</TableColumnHeader>
            </TableRow>
          </TableHeader>
          <TableBody>
            {budget.entries?.sort().map((entry) => (
              <TableRow key={entry.id}>
                <TableCell minW={60}>
                  {editingEntry?.id === entry.id ? (
                    <Input
                      size="sm"
                      value={editingEntry.name}
                      onChange={(e) =>
                        setEditingEntry((prev) =>
                          prev ? { ...prev, name: e.target.value } : null
                        )
                      }
                    />
                  ) : (
                    entry.name
                  )}
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
                    <HStack>
                      <Button
                        size="sm"
                        aria-label="Edit"
                        onClick={() =>
                          setEditingEntry({
                            id: entry.id,
                            name: entry.name,
                          })
                        }
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
        </HStack>
      </VStack>
    </Box>
  );
};
