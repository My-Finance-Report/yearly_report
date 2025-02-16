import { AccountsService } from "@/client"
import {
  Box,
  Button,
  Heading,
  Spinner,
  Table,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Input,
  IconButton,
  VStack,
  HStack,
} from "@chakra-ui/react"
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query"
import { useState } from "react"
import { EditIcon, CheckIcon, CloseIcon, DeleteIcon } from "@chakra-ui/icons"

interface CategoriesManagerProps {
  accountId: number
}

export const CategoriesManager = ({ accountId }: CategoriesManagerProps) => {
  const queryClient = useQueryClient()
  const [newCategory, setNewCategory] = useState("")
  const [editingCategory, setEditingCategory] = useState<{ id: number; name: string } | null>(null)

  const { data: categories, isLoading } = useQuery({
    queryKey: ["categories", accountId],
    queryFn: () => AccountsService.getCategories({ sourceId: accountId }),
  })

  const deleteCategoryMutation = useMutation({
    mutationFn: (categoryId: number) =>
      AccountsService.deleteCategory({ categoryId }),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ["categories", accountId] }),
  })

  const updateCategoryMutation = useMutation({
    mutationFn: ({ categoryId, name, sourceId }: { categoryId: number; name: string; sourceId: number}) =>
      AccountsService.updateCategory({ 
        categoryId, 
        requestBody: { name, source_id: sourceId} 
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] })
      setEditingCategory(null)
    },
  })

  const addCategoryMutation = useMutation({
    mutationFn: () => 
      AccountsService.createCategory({ sourceId: accountId, requestBody: { source_id: accountId, name: newCategory } }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["categories", accountId] })
      setNewCategory("")
    },
  })

  return (
    <Box>
      <Heading size="md" mb={4}>
        Categories
      </Heading>

      {isLoading ? (
        <Spinner />
      ) : (
        <VStack align="start" spacing={4} w="full">
          {/* Categories Table */}
          <Table size="sm" variant="simple">
            <Thead>
              <Tr>
                <Th>Name</Th>
                <Th textAlign="right">Actions</Th>
              </Tr>
            </Thead>
            <Tbody>
              {categories?.map((category) => (
                <Tr key={category.id}>
                  <Td>
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
                  </Td>
                  <Td textAlign="right">
                    {editingCategory?.id === category.id ? (
                      <HStack spacing={2} justify="flex-end">
                        <IconButton
                          size="sm"
                          colorScheme="green"
                          icon={<CheckIcon />}
                          aria-label="Save"
                          onClick={() =>
                            updateCategoryMutation.mutate({
                              categoryId: category.id,
                              name: editingCategory.name,
                              sourceId: accountId,
                            })
                          }
                        />
                        <IconButton
                          size="sm"
                          colorScheme="gray"
                          icon={<CloseIcon />}
                          aria-label="Cancel"
                          onClick={() => setEditingCategory(null)}
                        />
                      </HStack>
                    ) : (
                      <HStack spacing={2} justify="flex-end">
                        <IconButton
                          size="sm"
                          icon={<EditIcon />}
                          aria-label="Edit"
                          onClick={() => setEditingCategory({ id: category.id, name: category.name })}
                        />
                        <IconButton
                          size="sm"
                          colorScheme="red"
                          icon={<DeleteIcon />}
                          aria-label="Delete"
                          onClick={() => deleteCategoryMutation.mutate(category.id)}
                        />
                      </HStack>
                    )}
                  </Td>
                </Tr>
              ))}
            </Tbody>
          </Table>

          <HStack w="full">
            <Input
              size="sm"
              placeholder="New category name"
              value={newCategory}
              onChange={(e) => setNewCategory(e.target.value)}
            />
            <Button
              margin={2}
              padding={8}
              colorScheme="blue"
              onClick={() => addCategoryMutation.mutate()}
              isDisabled={!newCategory.trim()}
            >
              Add Category
            </Button>
          </HStack>
        </VStack>
      )}
    </Box>
  )
}
