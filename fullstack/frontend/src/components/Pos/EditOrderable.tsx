import { useState } from "react";
import {
  Box,
  Button,
  Flex,
  Heading,
  Stack,
  Table,
  Text,
  Input,
  FieldRoot,
  FieldLabel,

} from "@chakra-ui/react";
import { HiCheck, HiX } from "react-icons/hi";
import { OrderableInput, PosService,  VariantGroupOutput_Output } from "@/client";
import { Editable } from "./EditVariantGroupForm";
import { useMutation, useQueryClient } from "@tanstack/react-query";

interface EditOrderableProps {
  orderable: OrderableInput;
  variantGroups: VariantGroupOutput_Output[];
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}

export function EditOrderable({
  orderable,
  variantGroups,
  setEditing,
}: EditOrderableProps) {
  const [editedOrderable, setEditedOrderable] = useState<OrderableInput>(orderable);
  const queryClient = useQueryClient();

  const mutation = useMutation({
    mutationFn: () => PosService.createOrUpdateMenuItem({
      requestBody: editedOrderable,
    }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["orderables"] });
      setEditing(null);
    }
  });

  const onSave = () => {
    mutation.mutate();
  };

  const handleRemoveVariantGroup = (groupId: number) => {
    setEditedOrderable((prev) => ({
      ...prev,
      variantGroups: prev.variantGroups.filter(
        (group) => group.id !== groupId
      ),
    }));
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="md" mb={4}>
      <Stack p={2}>
        <Flex justify="space-between" align="center">
          <Heading size="sm">Edit Menu Item</Heading>
          <Button
            size="sm"
            colorPalette="red"
            variant="surface"
            onClick={() => setEditing(null)}
          >
            <HiX />
          </Button>
        </Flex>
        <FieldRoot mb={4}>
          <FieldLabel>Name</FieldLabel>
          <Input
            value={editedOrderable.name}
            onChange={(e) =>
              setEditedOrderable({ ...editedOrderable, name: e.target.value })
            }
            placeholder="Item Name"
          />
        </FieldRoot>
        <FieldRoot mb={4}>
          <FieldLabel>Price</FieldLabel>
          <Flex alignItems="center" gap={2}>
            <Text>$</Text>
            <Input
              value={parseFloat(editedOrderable.price as string)}
              type="number"
              step={0.01}
              onChange={(e) =>
                setEditedOrderable({
                  ...editedOrderable,
                  price: String(parseFloat(e.target.value) || 0),
                })
              }
            />
          </Flex>
        </FieldRoot>

        <Heading size="sm" mb={2}>Modifiers</Heading>
        <Table.Root size="sm" variant="outline">
          <Table.Header>
            <Table.Row>
              <Table.ColumnHeader>Group Name</Table.ColumnHeader>
              <Table.ColumnHeader>Required</Table.ColumnHeader>
              <Table.ColumnHeader>Modifiers</Table.ColumnHeader>
              <Table.ColumnHeader></Table.ColumnHeader>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {editedOrderable.variantGroups.map((group) => (
              <Table.Row key={group.id}>
                <Table.Cell>{group.name}</Table.Cell>
                <Table.Cell>{group.required ? "Yes" : "No"}</Table.Cell>
                <Table.Cell>{group.variants.length}</Table.Cell>
                <Table.Cell>
                  <Button
                    size="sm"
                    colorPalette="red"
                    variant="surface"
                    onClick={() => group.id && handleRemoveVariantGroup(group.id)}
                  >
                    <HiX />
                  </Button>
                </Table.Cell>
              </Table.Row>
            ))}
          </Table.Body>
        </Table.Root>
        <AddVariantGroupForm
          variantGroups={variantGroups}
          editedOrderable={editedOrderable}
          setEditedOrderable={setEditedOrderable}
        />
        <Stack direction="row" gap={2} mt={4}>
          <Button onClick={onSave}>
            <HiCheck /> Save
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}

function AddVariantGroupForm({
  variantGroups,
  editedOrderable,
  setEditedOrderable,
}: {
  variantGroups: VariantGroupOutput_Output[];
  editedOrderable: OrderableInput;
  setEditedOrderable: React.Dispatch<React.SetStateAction<OrderableInput>>;
}) {

  const addableGroups = variantGroups.filter((group) => !editedOrderable.variantGroups.some((g) => g.id === group.id));
  if (addableGroups.length === 0) {
    return null;
  }
  return (
    <Box p={4} borderWidth="1px" borderRadius="md" mb={4}>
      <Stack p={2}>
            <Heading size="sm">Add Modifier Group</Heading>
            {addableGroups.map((group) => (
          <Button
            key={group.id}
            size="sm"
            variant="surface"
            onClick={() => setEditedOrderable((prev) => ({ ...prev, variantGroups: [...prev.variantGroups, group] }))}
          >
            {group.name}
          </Button>
        ))}
      </Stack>
    </Box>
  );
}