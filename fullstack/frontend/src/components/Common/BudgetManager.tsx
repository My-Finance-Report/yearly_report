import {
  type BudgetCategoryLinkOut,
  type BudgetEntryOut,
  BudgetsService,
} from "@/client";

import BoxWithText from "./BoxWithText";
import { DeleteIcon, EditIcon } from "@chakra-ui/icons";
import {
  Button,
  HStack,
  TableBody,
  TableCell,
  Box,
  TableColumnHeader,
  TableHeader,
  Table,
  TableRow,
  Tag,
  Text,
  VStack,
  Flex,
} from "@chakra-ui/react";
import {
  type UseMutationResult,
  useMutation,
  useQueryClient,
} from "@tanstack/react-query";
import { useState } from "react";
import { FaPlus } from "react-icons/fa";
import type { BudgetStatus } from "../../client";
import { EditBudgetEntry, CreateBudgetEntry } from "./EditBudgetEntry";
import { formatCurrency } from "../Charting/PieChart";
import { useIsMobile } from "@/hooks/useIsMobile";

export const ManageBudget = ({
  budgetStatus,
}: {
  budgetStatus: BudgetStatus;
}) => {
  const queryClient = useQueryClient();

  const budgetEntryLookup: Record<BudgetEntryOut["id"], BudgetEntryOut> =
    budgetStatus.entries.reduce(
      (acc, entry) => {
        acc[entry.id] = entry;
        return acc;
      },
      {} as Record<BudgetEntryOut["id"], BudgetEntryOut>
    );

  const deleteEntryMutation = useMutation({
    mutationFn: (entryId: number) =>
      BudgetsService.deleteBudgetEntry({
        entryId: entryId,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["budgetStatus"] });
    },
  });

  const isMobile = useIsMobile();

  return (
    <VStack>
      <CreateNew budgetId={budgetStatus.budget_id} />
      {budgetStatus.entries.length > 0 ? (
        <Table.Root variant="outline" borderRadius="md">
          <TableHeader>
            <TableRow>
              <TableColumnHeader>Budget Entry</TableColumnHeader>
              <TableColumnHeader>
              Target <Text fontSize="xs">(/month)</Text>
            </TableColumnHeader>
            {!isMobile && <TableColumnHeader>Categories</TableColumnHeader>}
            <TableColumnHeader>Actions</TableColumnHeader>
          </TableRow>
        </TableHeader>
        <TableBody>
          {budgetStatus.entry_status?.sort().map((entry, index) => (
            <>
              <TableRow key={index}>
                <TableCell>{entry.name}</TableCell>
                <TableCell>{formatCurrency(Number(entry.amount))}</TableCell>
                {!isMobile && (
                  <TableCell>
                    {budgetEntryLookup[entry.id]?.category_links?.map(
                      (category) => (
                        <CategoryLink key={category.id} category={category} />
                      )
                    )}
                  </TableCell>
                )}
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
      ):(
        <Flex p={2} direction={"column"} alignItems="center" gap={2}>
        <Text textAlign="center">No Budget Entries Yet</Text>
        </Flex>
      )}
      <ProjectionCards budgetStatus={budgetStatus} />
    </VStack>
  );
};

function ProjectionCards({ budgetStatus }: { budgetStatus: BudgetStatus }) {
  const monthly = budgetStatus.entry_status.reduce(
    (acc, entry) => acc + Number(entry.amount),
    0
  );
  const yearly = monthly * 12;
  return (
    <Flex p={2} direction={"column"} alignItems="center" gap={2}>
      <Text fontSize="xl" fontWeight="bold">
        Totals
      </Text>
      <HStack mt={2}>
        <BoxWithText text="/ Month" isCollapsable={false} minW={40}>
          <Box p={2}>
            <Text fontSize="lg">{formatCurrency(monthly)}</Text>
          </Box>
        </BoxWithText>
        <BoxWithText text="/ Year" isCollapsable={false} minW={40}>
          <Box p={2}>
            <Text fontSize="lg">{formatCurrency(yearly)}</Text>
          </Box>
        </BoxWithText>
      </HStack>
    </Flex>
  );
}

function CreateNew({ budgetId }: { budgetId: number }) {
  const [newEntry, setNewEntry] = useState<BudgetEntryOut | null>(null);
  const [isOpen, setIsOpen] = useState(false);



  return (
    <HStack w="full">
      <Button size="sm" onClick={() => setIsOpen(true)}>
        <FaPlus />
        Add Entry
      </Button>

      <CreateBudgetEntry
          isOpen={isOpen}
          onClose={() => {
            setIsOpen(false);
            setNewEntry(null);
          }}
          budgetId={budgetId}
        />
    </HStack>
  );
}

function ActionsCell({
  entry,
  deleteEntryMutation,
  isMobile,
}: {
  entry: BudgetEntryOut;
  deleteEntryMutation: UseMutationResult<unknown, Error, number, unknown>;
  isMobile: boolean;
}) {
  const [isOpen, setIsOpen] = useState(false);
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
  );
}

function CategoryLink({ category }: { category: BudgetCategoryLinkOut }) {
  return (
    <Tag.Root size="sm" m={2}>
      <Text key={category.id}>{category.stylized_name}</Text>
    </Tag.Root>
  );
}
