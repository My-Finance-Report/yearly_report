import { createFileRoute } from '@tanstack/react-router'
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
} from '@chakra-ui/react'
import { useState } from 'react'
import { HiCheck, HiPlus, HiX } from 'react-icons/hi'

export const Route = createFileRoute('/_layout/_logged_in/manage-menu')({
  component: ManageMenu,
})

interface Variant {
  id: string
  name: string
  priceDelta: number
}

interface VariantGroup {
  id: string
  order: number
  name: string
  required: boolean
  variants: Variant[]
}

interface Orderable {
  id: string
  name: string
  price: number
  variantGroups: VariantGroup[]
}

function EditVariantGroup({ group, onChange, onDelete }: {
  group: VariantGroup
  onChange: (group: VariantGroup) => void
  onDelete: () => void
}) {
  const addVariant = () => {
    onChange({
      ...group,
      variants: [
        ...group.variants,
        { id: Math.random().toString(), name: '', priceDelta: 0 }
      ]
    })
  }

  const updateVariant = (index: number, variant: Variant) => {
    const newVariants = [...group.variants]
    newVariants[index] = variant
    onChange({ ...group, variants: newVariants })
  }

  const deleteVariant = (index: number) => {
    onChange({
      ...group,
      variants: group.variants.filter((_, i) => i !== index)
    })
  }

  return (
    <Box p={4} borderWidth="1px" borderRadius="md" mb={4}>
      <Stack p={2}>
        <Flex justify="space-between" align="center">
          <Heading size="sm"></Heading>
          <Button size="sm" colorPalette="red" variant="surface" onClick={onDelete}><HiX /> </Button>
        </Flex>
        <FieldRoot mb={4}>
          <FieldLabel>Name</FieldLabel>
          <Input
            value={group.name}
            onChange={e => onChange({ ...group, name: e.target.value })}
            placeholder="Group Name"
          />
        </FieldRoot>
        <FieldRoot display="flex" alignItems="center" gap={2} mb={2}>
          <FieldLabel mb={0}>Required</FieldLabel>
          <Switch.Root 
            checked={group.required}
            onCheckedChange={(details: any) => onChange({ ...group, required: details.checked })}
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
                    onChange={e => updateVariant(index, { ...variant, name: e.target.value })}
                  />
                </Table.Cell>
                <Table.Cell>
                  <NumberInput.Root size="sm">
                    <Flex alignItems="center" gap={2}>
                    <NumberInput.Label>$</NumberInput.Label>
                    <NumberInput.Input
                      value={variant.priceDelta}
                      onChange={(e: any) => updateVariant(index, { ...variant, priceDelta: Number(e.target.value) })}
                    />
                    </Flex>
                  </NumberInput.Root>
                </Table.Cell>
                <Table.Cell>
                  <Button size="sm" colorPalette="red" variant="surface" onClick={() => deleteVariant(index)}><HiX /></Button>
                </Table.Cell>
              </Table.Row>
            ))}
          </Table.Body>
        </Table.Root>
        <Button onClick={addVariant}>Add Modifier</Button>
      </Stack>
    </Box>
  )
}

function EditOrderable({ orderable, onSave, onCancel }: {
  orderable?: Orderable
  onSave: (orderable: Orderable) => void
  onCancel: () => void
}) {
  const [item, setItem] = useState<Orderable>(orderable || {
    id: Math.random().toString(),
    name: '',
    price: 0,
    variantGroups: []
  })

  const addVariantGroup = () => {
    setItem({
      ...item,
      variantGroups: [
        ...item.variantGroups,
        {
          id: Math.random().toString(),
          order: item.variantGroups.length,
          name: '',
          required: false,
          variants: []
        }
      ]
    })
  }

  const updateVariantGroup = (index: number, group: VariantGroup) => {
    const newGroups = [...item.variantGroups]
    newGroups[index] = group
    setItem({ ...item, variantGroups: newGroups })
  }

  const deleteVariantGroup = (index: number) => {
    setItem({
      ...item,
      variantGroups: item.variantGroups.filter((_, i) => i !== index)
    })
  }

  return (
    <Stack p={4}>
      <Heading size="md">{orderable ? 'Edit Item' : 'New Item'}</Heading>
        <FieldRoot>
        <FieldLabel>Name</FieldLabel>
        <Input 
          value={item.name} 
          onChange={e => setItem({ ...item, name: e.target.value })}
        />
      </FieldRoot>
      <FieldRoot>
        <FieldLabel>Base Price ($)</FieldLabel>
        <NumberInput.Root>
          <NumberInput.Input
            value={item.price}
            onChange={(e: any) => setItem({ ...item, price: Number(e.target.value) })}
          />
        </NumberInput.Root>
      </FieldRoot>
      <Box>
        <Heading size="sm" mb={4}>Modifier Groups</Heading>
        {item.variantGroups.map((group, index) => (
          <EditVariantGroup
            key={group.id}
            group={group}
            onChange={group => updateVariantGroup(index, group)}
            onDelete={() => deleteVariantGroup(index)}
          />
        ))}
        <Button variant="outline" onClick={addVariantGroup}><HiPlus /> Modifier Group</Button>
      </Box>
      <Flex gap={4} mt={8} justifyContent="center">
        <Button  onClick={() => onSave(item)}>Save</Button>
        <Button variant="outline" onClick={onCancel}>Cancel</Button>
      </Flex>
    </Stack>
  )
}

function ManageMenu() {
  const [orderables, setOrderables] = useState<Orderable[]>([
    {
      id: "1",
      name: "coffee",
      price: 2,
      variantGroups: [
        {
          id: "0",
          order: 0,
          name: "temp",
          required: true,
          variants: [
            { id: "1", name: "Hot", priceDelta: 0 },
            { id: "2", name: "Cold", priceDelta: 1 },
          ]
        },
        {
          id: "1",
          order: 1,
          name: "size",
          required: true,
          variants: [
            { id: "1", name: "Small", priceDelta: 0 },
            { id: "2", name: "Medium", priceDelta: 1 },
            { id: "3", name: "Large", priceDelta: 2 },
          ]
        }
      ]
    }
  ])

  const [editingItem, setEditingItem] = useState<Orderable | undefined>()

  const handleSave = (item: Orderable) => {
    if (editingItem) {
      setOrderables(prev => prev.map(o => o.id === item.id ? item : o))
    } else {
      setOrderables(prev => [...prev, item])
    }
    setEditingItem(undefined)
  }

  const handleDelete = (id: string) => {
    setOrderables(prev => prev.filter(o => o.id !== id))
  }

  if (editingItem || editingItem === null) {
    return (
      <EditOrderable
        orderable={editingItem}
        onSave={handleSave}
        onCancel={() => setEditingItem(undefined)}
      />
    )
  }

  return (
    <Box p={4}>
      <Flex justify="space-between" align="center" mb={4}>
        <Heading>Menu</Heading>
        <Button variant="surface" onClick={() => setEditingItem(undefined)}><HiPlus /> New Item</Button>
      </Flex>
      <Table.Root>
        <Table.Header>
          <Table.Row>
            <Table.ColumnHeader>Name</Table.ColumnHeader>
            <Table.ColumnHeader>Base Price</Table.ColumnHeader>
            <Table.ColumnHeader>Modifiers</Table.ColumnHeader>
            <Table.ColumnHeader></Table.ColumnHeader>
          </Table.Row>
        </Table.Header>
        <Table.Body>
          {orderables.map(item => (
            <Table.Row key={item.id}>
              <Table.Cell>{item.name}</Table.Cell>
              <Table.Cell>${item.price.toFixed(2)}</Table.Cell>
              <Table.Cell>
                <Text fontSize="sm">
                  {item.variantGroups.map(g => g.name).join(', ')}
                </Text>
              </Table.Cell>
              <Table.Cell>
                <Flex gap={2}>
                  <Button size="sm" onClick={() => setEditingItem(item)}>Edit</Button>
                  <Button size="sm" colorPalette="red" variant="surface" onClick={() => handleDelete(item.id)}>Delete</Button>
                </Flex>
              </Table.Cell>
            </Table.Row>
          ))}
        </Table.Body>
      </Table.Root>
    </Box>
  )
}
