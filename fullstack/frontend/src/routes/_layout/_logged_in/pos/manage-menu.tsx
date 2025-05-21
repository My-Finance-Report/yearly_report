import { createFileRoute } from "@tanstack/react-router";
import {
  Box,
  Button,
  Flex,
  Heading,
  Table,
  Text,
  Breadcrumb,
} from "@chakra-ui/react";
import { useState } from "react";
import { EditOrderable } from "@/components/Pos/EditOrderable";
import {
  Editable,
  EditVariantGroupForm,
} from "@/components/Pos/EditVariantGroupForm";

import { HiPencil, HiPlus, HiX } from "react-icons/hi";
import { useQuery } from "@tanstack/react-query";
import {
  PosService,
  OrderableOutput_Output,
  VariantGroupOutput_Output,
} from "@/client";

export const Route = createFileRoute("/_layout/_logged_in/pos/manage-menu")({
  component: ManageMenu,
});

function BreadcrumbComponent({ page }: { page: "menu" | "modifiers" }) {
  return (
    <Breadcrumb.Root size="lg">
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos/manage-menu">Menu</Breadcrumb.Link>
        </Breadcrumb.Item>
        {page === "modifiers" && (
          <>
            <Breadcrumb.Separator />
            <Breadcrumb.Item>
              <Breadcrumb.CurrentLink>Modifier Groups</Breadcrumb.CurrentLink>
            </Breadcrumb.Item>
          </>
        )}
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}

export function ManageMenu() {
  const { data: orderables } = useQuery({
    queryKey: ["orderables"],
    queryFn: () => PosService.getMenu(),
  });

  const { data: groups } = useQuery({
    queryKey: ["variantgroups"],
    queryFn: () => PosService.getVariantGroups(),
  });

  const [editing, setEditing] = useState<Editable | null>(null);

  return (
    <Box p={4}>
      <BreadcrumbComponent page="menu" />
      <Flex justify="space-between" align="center" mb={4}>
        <Button
          variant="surface"
          onClick={() =>
            setEditing({
              type: "variantGroup",
              data: {
                id: null,
                name: "",
                order_of_appearance: 0,
                required: false,
                variants: [],
              },
            })
          }
        >
          <HiPlus /> New Modifier Group
        </Button>
        <Button
          variant="surface"
          onClick={() =>
            setEditing({
              type: "orderable",
              data: { id: null, name: "", price: "0", variantGroups: [] },
            })
          }
        >
          <HiPlus /> New Item
        </Button>
      </Flex>
      <Flex direction={"column"} gap={4}>
        {editing ? (
          <Editor
            editing={editing}
            variantGroups={groups || []}
            setEditing={setEditing}
          />
        ) : (
          <Displays
            orderables={orderables}
            groups={groups}
            setEditing={setEditing}
          />
        )}
      </Flex>
    </Box>
  );
}

function Editor({
  editing,
  variantGroups,
  setEditing,
}: {
  editing: Editable;
  variantGroups: VariantGroupOutput_Output[];
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}) {
  if (editing.type === "variantGroup") {
    return (
      <EditVariantGroupForm group={editing.data} setEditing={setEditing} />
    );
  } else if (editing.type === "orderable") {
    return (
      <EditOrderable
        orderable={editing.data}
        variantGroups={variantGroups}
        setEditing={setEditing}
      />
    );
  } else {
    throw new Error("Invalid editing type");
  }
}

function Displays({
  orderables,
  groups,
  setEditing,
}: {
  orderables: OrderableOutput_Output[] | undefined;
  groups: VariantGroupOutput_Output[] | undefined;
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}) {
  return (
    <>
      {orderables ? (
        <Orderables orderables={orderables} setEditing={setEditing} />
      ) : (
        <Text>Loading...</Text>
      )}
      {groups ? (
        <Variants groups={groups} setEditing={setEditing} />
      ) : (
        <Text>Loading...</Text>
      )}
    </>
  );
}

function Variants({
  groups,
  setEditing,
}: {
  groups: VariantGroupOutput_Output[];
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}) {
  const handleDeleteGroup = (id: number) => {
    PosService.deleteVariantGroup({ groupId: id });
    setEditing(null);
  };

  return (
    <>
      <Flex justify="space-between" align="center" mb={4}>
        <Heading>Modifier Groups</Heading>
      </Flex>
      <Table.Root variant="outline">
        <Table.Header>
          <Table.Row>
            <Table.ColumnHeader>Name</Table.ColumnHeader>
            <Table.ColumnHeader>Required</Table.ColumnHeader>
            <Table.ColumnHeader>Modifiers</Table.ColumnHeader>
            <Table.ColumnHeader>Actions</Table.ColumnHeader>
          </Table.Row>
        </Table.Header>
        <Table.Body>
          {groups.map((group) => (
            <Table.Row key={group.id}>
              <Table.Cell>{group.name}</Table.Cell>
              <Table.Cell>{group.required ? "Yes" : "No"}</Table.Cell>
              <Table.Cell>
                <Text fontSize="sm">
                  {group.variants
                    .slice(0, 3)
                    .map((v) => v.name)
                    .join(", ")}
                  {group.variants.length > 3 && "..."}
                </Text>
              </Table.Cell>
              <Table.Cell>
                <Flex gap={2}>
                  <Button
                    size="sm"
                    variant="surface"
                    onClick={() =>
                      setEditing({ type: "variantGroup", data: group })
                    }
                  >
                    <HiPencil />
                  </Button>
                  <Button
                    size="sm"
                    colorPalette="red"
                    variant="surface"
                    onClick={() => group.id && handleDeleteGroup(group.id)}
                  >
                    <HiX />
                  </Button>
                </Flex>
              </Table.Cell>
            </Table.Row>
          ))}
        </Table.Body>
      </Table.Root>
    </>
  );
}

function Orderables({
  orderables,
  setEditing,
}: {
  orderables: OrderableOutput_Output[];
  setEditing: React.Dispatch<React.SetStateAction<Editable | null>>;
}) {
  const handleDelete = (id: number) => {
    PosService.deleteMenuItem({ orderableId: id });
    setEditing(null);
  };
  return (
    <>
      <Flex justify="space-between" align="center" mb={4}>
        <Heading>Menu</Heading>
      </Flex>
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
                    onClick={() =>
                      setEditing({ type: "orderable", data: item })
                    }
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
    </>
  );
}
