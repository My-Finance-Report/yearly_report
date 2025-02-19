import { AccountsService } from "@/client";

import {
  Box,
  Button,
  Heading,
  Spinner,
  Input,
  VStack,
  HStack,
  TableRoot,
  TableHeader,
  TableBody,
  TableRow,
  TableColumnHeader,
  TableCell,
} from "@chakra-ui/react";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { useState } from "react";
import { EditIcon, CheckIcon, CloseIcon, DeleteIcon } from "@chakra-ui/icons";
import { FaPlus } from "react-icons/fa";

interface CategoriesManagerProps {
  accountId: number;
}

export const CategoriesManager = ({ accountId }: CategoriesManagerProps) => {
  const queryClient = useQueryClient();
  const [newCategory, setNewCategory] = useState("");
  const [editingCategory, setEditingCategory] = useState<{ id: number; name: string } | null>(null);

  const { data: categories, isLoading } = useQuery({
    queryKey: ["categories", accountId],
    queryFn: () => AccountsService.getCategories({ sourceId: accountId }),
  });

  const deleteCategoryMutation = useMutation({
    mutationFn: (categoryId: number) => AccountsService.deleteCategory({ categoryId }),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ["categories", accountId] }),
  });

  const updateCategoryMutation = useMutation({
    mutationFn: ({ categoryId, name, sourceId }: { categoryId: number; name: string; sourceId: number }) =>
      AccountsService.updateCategory({ categoryId, requestBody: { name, source_id: sourceId } }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] });
      setEditingCategory(null);
    },
  });

  const addCategoryMutation = useMutation({
    mutationFn: () =>
      AccountsService.createCategory({ sourceId: accountId, requestBody: { source_id: accountId, name: newCategory } }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] });
      setNewCategory("");
    },
  });

  return (
    <Box>
      {isLoading ? (
        <Spinner />
      ) : (
        <VStack gap={4}>
          <TableRoot variant="outline">
            <TableHeader>
              <TableRow>
                <TableColumnHeader>Category</TableColumnHeader>
                <TableColumnHeader>Actions</TableColumnHeader>
              </TableRow>
            </TableHeader>
            <TableBody>
              {categories?.sort().map((category) => (
                <TableRow key={category.id}>
                  <TableCell minW={60}>
                    {editingCategory?.id === category.id ? (
                      <Input
                        size="sm"
                        value={editingCategory.name}
                        onChange={(e) =>
                          setEditingCategory((prev) =>
                            prev ? { ...prev, name: e.target.value } : null
                          )
                        }
                      />
                    ) : (
                      category.name
                    )}
                  </TableCell>
                  <TableCell textAlign="right">
                    {editingCategory?.id === category.id ? (
                      <HStack>
                        <Button
                          size="sm"
                          aria-label="Save"
                          onClick={() =>
                            updateCategoryMutation.mutate({
                              categoryId: category.id,
                              name: editingCategory.name,
                              sourceId: accountId,
                            })
                          }
                        > <CheckIcon />
                          Save
                        </Button>
                        <Button
                          size="sm"
                          aria-label="Cancel"
                          onClick={() => setEditingCategory(null)}
                        >
                          <CloseIcon />

                        </Button>
                      </HStack>
                    ) : (
                      <HStack>
                        <Button
                          size="sm"
                          aria-label="Edit"
                          onClick={() => setEditingCategory({ id: category.id, name: category.name })}
                        >
                          <EditIcon /> Edit
                        </Button>
                        <Button
                          size="sm"
                          colorScheme="red"
                          aria-label="Delete"
                          onClick={() => deleteCategoryMutation.mutate(category.id)}
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
              placeholder="New category name"
              value={newCategory}
              onChange={(e) => setNewCategory(e.target.value)}
            />
            <Button
              size="sm"
              onClick={() => addCategoryMutation.mutate()}
              disabled={!newCategory.trim()}
            >
              <FaPlus />
              Add Category
            </Button>
          </HStack>
          <Heading size="sm">Note: if you add or remove a category we will recategorize your transactions</Heading>
        </VStack>
      )}
    </Box>
  );
};


