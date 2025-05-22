import React, { useState } from "react";
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
  Switch,
} from "@chakra-ui/react";
import { HiCheck, HiPlus, HiX } from "react-icons/hi";
import {
  OrderableInput,
  PosService,
  VariantGroupInput,
  VariantBase_Input,
} from "@/client";
import { useMutation, useQueryClient } from "@tanstack/react-query";

export type Editable =
  | { type: "orderable"; data: OrderableInput }
  | { type: "variantGroup"; data: VariantGroupInput };

interface EditVariantGroupFormProps {
  group: VariantGroupInput;
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}

export function EditVariantGroupForm({
  group,
  setEditing,
}: EditVariantGroupFormProps) {
  const [editedGroup, setEditedGroup] = useState<VariantGroupInput>(group);
  const queryClient = useQueryClient();

  const mutation = useMutation({
    mutationFn: () =>
      PosService.createOrUpdateVariantGroup({
        requestBody: editedGroup,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["variantgroups"] });
      setEditing(null);
    },
  });

  const onSave = () => {
    mutation.mutate();
  };

  const addVariant = () => {
    setEditedGroup({
      ...editedGroup,
      variants: [
        ...editedGroup.variants,
        { id: null, name: "", price_delta: "0" },
      ],
    });
  };
  const updateVariant = (index: number, variant: VariantBase_Input) => {
    const newVariants = [...editedGroup.variants];
    newVariants[index] = variant;
    setEditedGroup({ ...editedGroup, variants: newVariants });
  };
  const deleteVariant = (index: number) => {
    setEditedGroup({
      ...editedGroup,
      variants: editedGroup.variants.filter((_, i) => i !== index),
    });
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="md" mb={4}>
      <Stack p={2}>
        <Flex justify="space-between" align="center">
          <Heading size="sm">Edit Group</Heading>
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
            value={editedGroup.name}
            onChange={(e) =>
              setEditedGroup({ ...editedGroup, name: e.target.value })
            }
            placeholder="Group Name"
          />
        </FieldRoot>
        <FieldRoot display="flex" alignItems="center" gap={2} mb={2}>
          <FieldLabel mb={0}>Required</FieldLabel>
          <Switch.Root
            checked={editedGroup.required}
            onCheckedChange={(details) =>
              setEditedGroup({ ...editedGroup, required: details.checked })
            }
          >
            <Switch.HiddenInput />
            <Switch.Control>
              <Switch.Thumb>
                <Switch.ThumbIndicator fallback={<HiX color="black" />}>
                  <HiCheck />
                </Switch.ThumbIndicator>
              </Switch.Thumb>
            </Switch.Control>
            <Switch.Label />
          </Switch.Root>
        </FieldRoot>
        <Table.Root size="sm" variant="outline">
          <Table.Header>
            <Table.Row>
              <Table.ColumnHeader>Modifier Name</Table.ColumnHeader>
              <Table.ColumnHeader>Price Change</Table.ColumnHeader>
              <Table.ColumnHeader></Table.ColumnHeader>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {editedGroup.variants.map((variant, index) => (
              <Table.Row key={index}>
                <Table.Cell>
                  <Input
                    size="sm"
                    value={variant.name}
                    onChange={(e) =>
                      updateVariant(index, { ...variant, name: e.target.value })
                    }
                  />
                </Table.Cell>
                <Table.Cell>
                  <Flex alignItems="center" gap={2}>
                    <Text>$</Text>
                    <Input
                      size="sm"
                      value={parseFloat(variant.price_delta as string) || 0}
                      type="number"
                      step={0.01}
                      onChange={(e) =>
                        updateVariant(index, {
                          ...variant,
                          price_delta: String(parseFloat(e.target.value) || 0),
                        })
                      }
                    />
                  </Flex>
                </Table.Cell>
                <Table.Cell>
                  <Button
                    size="sm"
                    colorPalette="red"
                    variant="surface"
                    onClick={() => deleteVariant(index)}
                  >
                    <HiX />
                  </Button>
                </Table.Cell>
              </Table.Row>
            ))}
          </Table.Body>
        </Table.Root>
        <Stack direction="row" gap={2} mt={4}>
          <Button onClick={() => onSave()}>
            <HiCheck /> Save
          </Button>
          <Button onClick={addVariant} variant="outline">
            <HiPlus /> Add Modifier
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}
