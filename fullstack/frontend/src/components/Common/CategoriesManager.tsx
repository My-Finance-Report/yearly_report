import { AccountsService } from "@/client"

import {
  Box,
  Button,
  HStack,
  Input,
  Spinner,
  TableBody,
  TableCell,
  TableColumnHeader,
  TableHeader,
  TableRoot,
  TableRow,
  VStack,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { useState } from "react"
import { FaEdit, FaPlus, FaSave, FaWindowClose } from "react-icons/fa"

interface CategoriesManagerProps {
  accountId: number
}

export const CategoriesManager = ({ accountId }: CategoriesManagerProps) => {
  const queryClient = useQueryClient()
  const [newCategory, setNewCategory] = useState("")
  const [editingCategory, setEditingCategory] = useState<{
    id: number
    name: string
  } | null>(null)

  const { data: categories, isLoading } = useQuery({
    queryKey: ["categories", accountId],
    queryFn: () => AccountsService.getCategories({ sourceId: accountId }),
  })

  const updateCategoryMutation = useMutation({
    mutationFn: ({
      categoryId,
      name,
      sourceId,
    }: { categoryId: number; name: string; sourceId: number }) =>
      AccountsService.updateCategory({
        categoryId,
        requestBody: { name, source_id: sourceId },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] })
      setEditingCategory(null)
    },
  })

  const addCategoryMutation = useMutation({
    mutationFn: () =>
      AccountsService.createCategory({
        sourceId: accountId,
        requestBody: { source_id: accountId, name: newCategory },
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] })
      setNewCategory("")
    },
  })

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
                            prev ? { ...prev, name: e.target.value } : null,
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
                        >
                          {" "}
                          <FaSave />
                          Save
                        </Button>
                        <Button
                          size="sm"
                          aria-label="Cancel"
                          onClick={() => setEditingCategory(null)}
                        >
                          <FaWindowClose />
                        </Button>
                      </HStack>
                    ) : (
                      <HStack>
                        <Button
                          size="sm"
                          aria-label="Edit"
                          onClick={() =>
                            setEditingCategory({
                              id: category.id,
                              name: category.name,
                            })
                          }
                        >
                          <FaEdit /> Edit
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
        </VStack>
      )}
    </Box>
  )
}
