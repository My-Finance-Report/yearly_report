import { createFileRoute } from "@tanstack/react-router";
import {
  Box,
  Button,
  Flex,
  FieldLabel,
  FieldRoot,
  Heading,
  Input,
  NumberInput,
  Stack,
  Switch,
  Table,
  Text,
  Breadcrumb,
} from "@chakra-ui/react";
import { useState } from "react";
import { HiCheck, HiPencil, HiPlus, HiX } from "react-icons/hi";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import {
  OrderableBase_Output,
  PosService,
  VariantBase_Output,
  VariantGroupBase_Output,
} from "@/client";

export const Route = createFileRoute("/_layout/_logged_in/manage-menu")({
  component: ManageMenu,
});

type Orderable = OrderableBase_Output;
type VariantGroup = VariantGroupBase_Output;
type Variant = VariantBase_Output;

function EditVariantGroup({
  group,
  onChange,
  onDelete,
}: {
  group: VariantGroup;
  onChange: (group: VariantGroup) => void;
  onDelete: () => void;
}) {
  const addVariant = () => {
    onChange({
      ...group,
      variants: [...group.variants, { id: 0, name: "", priceDelta: "0" }],
    });
  };

  const updateVariant = (index: number, variant: Variant) => {
    const newVariants = [...group.variants];
    newVariants[index] = variant;
    onChange({ ...group, variants: newVariants });
  };

  const deleteVariant = (index: number) => {
    onChange({
      ...group,
      variants: group.variants.filter((_, i) => i !== index),
    });
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="md" mb={4}>
      <Stack p={2}>
        <Flex justify="space-between" align="center">
          <Heading size="sm"></Heading>
          <Button
            size="sm"
            colorPalette="red"
            variant="surface"
            onClick={onDelete}
          >
            <HiX />{" "}
          </Button>
        </Flex>
        <FieldRoot mb={4}>
          <FieldLabel>Name</FieldLabel>
          <Input
            value={group.name}
            onChange={(e) => onChange({ ...group, name: e.target.value })}
            placeholder="Group Name"
          />
        </FieldRoot>
        <FieldRoot display="flex" alignItems="center" gap={2} mb={2}>
          <FieldLabel mb={0}>Required</FieldLabel>
          <Switch.Root
            checked={group.required}
            onCheckedChange={(details) =>
              onChange({ ...group, required: details.checked })
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
            {group.variants.map((variant, index) => (
              <Table.Row key={variant.id}>
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
                      value={parseFloat(variant.priceDelta) || 0}
                      type="number"
                      onChange={(e) =>
                        updateVariant(index, {
                          ...variant,
                          priceDelta: String(parseFloat(e.target.value) || 0),
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
        <Button onClick={addVariant}>Add Modifier</Button>
      </Stack>
    </Box>
  );
}

function EditOrderable({
  orderable,
  onSave,
  onCancel,
}: {
  orderable: Orderable | null;
  onSave: (orderable: Orderable) => void;
  onCancel: () => void;
}) {
  const [item, setItem] = useState<Orderable>(
    orderable ?? {
      id: 0,
      name: "",
      price: "0",
      variantGroups: [],
    },
  );

  const addVariantGroup = () => {
    const newGroup: VariantGroup = {
      id: 0,
      name: "",
      required: false,
      variants: [],
      order_of_appearance: item.variantGroups.length,
    };
    setItem({
      ...item,
      variantGroups: [...item.variantGroups, newGroup],
    });
  };

  const updateVariantGroup = (index: number, group: VariantGroup) => {
    const newGroups = [...item.variantGroups];
    newGroups[index] = group;
    setItem({ ...item, variantGroups: newGroups });
  };

  const deleteVariantGroup = (index: number) => {
    setItem({
      ...item,
      variantGroups: item.variantGroups.filter((_, i) => i !== index),
    });
  };

  return (
    <Stack p={4}>
      <Heading size="md">{item.id ? "Edit Item" : "New Item"}</Heading>
      <FieldRoot>
        <FieldLabel>Name</FieldLabel>
        <Input
          value={item.name}
          onChange={(e) => setItem({ ...item, name: e.target.value })}
        />
      </FieldRoot>
      <FieldRoot>
        <FieldLabel>Base Price ($)</FieldLabel>
        <NumberInput.Root>
          <NumberInput.Input
            value={Number(item.price)}
            onChange={(e) =>
              setItem({ ...item, price: String(Number(e.target.value)) })
            }
          />
        </NumberInput.Root>
      </FieldRoot>
      <Box>
        <Heading size="sm" mb={4}>
          Modifier Groups
        </Heading>
        {item.variantGroups.map((group, index) => (
          <EditVariantGroup
            key={group.id}
            group={group}
            onChange={(group) => updateVariantGroup(index, group)}
            onDelete={() => deleteVariantGroup(index)}
          />
        ))}
        <Button variant="outline" onClick={addVariantGroup}>
          <HiPlus /> Modifier Group
        </Button>
      </Box>
      <Flex gap={2} mt={4}>
        <Button onClick={() => onSave(item)}>
          <HiCheck /> Save
        </Button>
        <Button
          variant="surface"
          onClick={() => {
            setItem(
              orderable ?? {
                id: 0,
                name: "",
                price: "0",
                variantGroups: [],
              },
            );
            onCancel();
          }}
        >
          <HiX /> Cancel
        </Button>
      </Flex>
    </Stack>
  );
}

function ManageMenu() {
  const queryClient = useQueryClient();
  const { data: orderables } = useQuery({
    queryKey: ["orderables"],
    queryFn: () => PosService.getMenu(),
  });
  const [editingItem, setEditingItem] = useState<Orderable | null>(null);

  const handleSave = async (item: Orderable) => {
    try {
      if (editingItem) {
        await PosService.updateMenuItem({
          orderableId: item.id || 0,
          requestBody: item,
        });
      } else {
        await PosService.createMenuItem({
          requestBody: item,
        });
      }
      queryClient.invalidateQueries({ queryKey: ["orderables"] });
      setEditingItem(null);
    } catch (error) {
      console.error("Failed to save item:", error);
    }
  };

  const handleDelete = async (id: number) => {
    try {
      await PosService.deleteMenuItem({
        orderableId: id,
      });
      queryClient.invalidateQueries({ queryKey: ["orderables"] });
    } catch (error) {
      console.error("Failed to delete item:", error);
    }
  };

  return (
    <Box p={4}>
      <BreadcrumbComponent />
      {editingItem ? (
        <EditOrderable
          orderable={editingItem}
          onSave={handleSave}
          onCancel={() => {
            setEditingItem(null);
            queryClient.invalidateQueries({ queryKey: ["orderables"] });
          }}
        />
      ) : (
        <>
          <Flex justify="space-between" align="center" mb={4}>
            <Heading>Menu</Heading>
            <Button
              variant="surface"
              onClick={() =>
                setEditingItem({
                  id: 0,
                  name: "",
                  price: "0",
                  variantGroups: [],
                })
              }
            >
              <HiPlus /> New Item
            </Button>
          </Flex>
          {orderables ? (
            <Table.Root variant="outline">
              <Table.Header>
                <Table.Row>
                  <Table.ColumnHeader>Name</Table.ColumnHeader>
                  <Table.ColumnHeader>Price</Table.ColumnHeader>
                  <Table.ColumnHeader>Modifiers</Table.ColumnHeader>
                  <Table.ColumnHeader>Actions</Table.ColumnHeader>
                </Table.Row>
              </Table.Header>
              <Table.Body>
                {orderables.map((item) => (
                  <Table.Row key={item.id}>
                    <Table.Cell>{item.name}</Table.Cell>
                    <Table.Cell>${Number(item.price).toFixed(2)}</Table.Cell>
                    <Table.Cell>
                      <Text fontSize="sm">
                        {item.variantGroups.map((g) => g.name).join(", ")}
                      </Text>
                    </Table.Cell>
                    <Table.Cell>
                      <Flex gap={2}>
                        <Button
                          size="sm"
                          variant="surface"
                          onClick={() => setEditingItem(item)}
                        >
                          <HiPencil />
                        </Button>
                        <Button
                          size="sm"
                          colorPalette="red"
                          variant="surface"
                          onClick={() => item.id && handleDelete(item.id)}
                        >
                          <HiX />
                        </Button>
                      </Flex>
                    </Table.Cell>
                  </Table.Row>
                ))}
              </Table.Body>
            </Table.Root>
          ) : (
            <Text>Loading...</Text>
          )}
        </>
      )}
    </Box>
  );
}

function BreadcrumbComponent() {
  return (
    <Breadcrumb.Root size="lg">
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Manage Menu</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
