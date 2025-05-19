import { createFileRoute } from '@tanstack/react-router'
import { Box, Flex, Text, Button, Heading, NumberInput, Breadcrumb } from '@chakra-ui/react'
import { useState } from 'react';

interface Variant {
    id: string;
    name: string;
    priceDelta: number;
}

interface VariantGroup {
    id: string;
    order: number;
    name: string;
    required: boolean;
    variants: Variant[];
}

interface Orderable {
    id: string;
    name: string;
    variantGroups: VariantGroup[];
    price: number;
}

interface SelectedVariant extends Variant {
    groupId: string;
}

interface OrderItem {
    orderable: Orderable;
    variants: SelectedVariant[];
    quantity: number;
}

interface CompleteOrder {
    id: string;
    timestamp: string;
    orderItems: OrderItem[];
}


export const Route = createFileRoute('/_layout/_logged_in/order')({
  component: Order,
})


const orderables: Orderable[] = [
    {
        id: "1",
        name: "coffee",
        variantGroups: [
            { id: "0", order: 0, name: "temp", required: true, variants: [
                { id: "1", name: "Hot", priceDelta: 0 },
                { id: "2", name: "Cold", priceDelta: 1 },
            ] },
            { id: "1", order: 1, name: "size", required: true, variants: [
                { id: "1", name: "Small", priceDelta: 0 },
                { id: "2", name: "Medium", priceDelta: 1 },
                { id: "3", name: "Large", priceDelta: 2 },
            ] },
            { id: "2", order: 2, name: "misc", required: false, variants: [
                { id: "1", name: "Extra Shot", priceDelta: 1 },
                { id: "2", name: "Decaf", priceDelta: 1 },
                { id: "3", name: "Add Milk", priceDelta: 1 },
            ] },
            { id: "3", order: 3, name: "flavors", required: false, variants: [
                { id: "1", name: "Vanilla", priceDelta: 1 },
                { id: "2", name: "Hazelnut", priceDelta: 1 },
                { id: "3", name: "Caramel", priceDelta: 1 },
                { id: "4", name: "Pumpkin", priceDelta: 1 },
                { id: "5", name: "Cookie", priceDelta: 1 },
                { id: "6", name: "Mocha", priceDelta: 1 },
                { id: "7", name: "Caramel Macchiato", priceDelta: 1 },
                { id: "8", name: "Hazelnut Macchiato", priceDelta: 1 },
                { id: "9", name: "Vanilla Macchiato", priceDelta: 1 },
                { id: "10", name: "Pumpkin Macchiato", priceDelta: 1 },
                { id: "11", name: "Cookie Macchiato", priceDelta: 1 },
                { id: "12", name: "Mocha Macchiato", priceDelta: 1 },
            ] },
        ],
        price: 2,
    },
]

function QuantitySelector({ setQuantity, quantity }: { setQuantity: React.Dispatch<React.SetStateAction<number>> , quantity: number}) {
    return (
<NumberInput.Root  width="200px"
value={String(quantity)}
onValueChange={(e) => setQuantity(Number(e.value))}
>
      <NumberInput.Control />
      <NumberInput.Input />
    </NumberInput.Root>
    )
}

function OrderableCard({ setInProgressOrder, orderable }: { setInProgressOrder: React.Dispatch<React.SetStateAction<Orderable | null>>, orderable: Orderable }) {
    return (<Box cursor="pointer" minH={100} onClick={() => setInProgressOrder(orderable)} display={"flex"} flexDirection={"column"} justifyContent="center" alignItems="center" p={2} minW={200} border="1px solid #ccc" borderRadius={4}>
        <Text cursor="pointer">{orderable.name}</Text>
    </Box>)
}

function InOrderCard({ setOrder, orderItem }: { setOrder: React.Dispatch<React.SetStateAction<CompleteOrder>>, orderItem: OrderItem }) {
    const totalPrice = orderItem.orderable.price + 
        orderItem.variants.reduce((sum, variant) => sum + variant.priceDelta, 0)
    
    const handleRemove = () => {
        setOrder(prev => ({
            ...prev,
            orderItems: prev.orderItems.filter(item => 
                !(item.orderable.id === orderItem.orderable.id && 
                  JSON.stringify(item.variants) === JSON.stringify(orderItem.variants))
            )
        }))
    }

    const variantsByGroup = orderItem.orderable.variantGroups.map(group => {
        const variantsInGroup = orderItem.variants.filter(selectedVariant => 
            'groupId' in selectedVariant && selectedVariant.groupId === group.id
        )
        const groupPriceDelta = variantsInGroup.reduce((sum, v) => sum + v.priceDelta, 0)
        return {
            groupName: group.name,
            variants: variantsInGroup,
            priceDelta: groupPriceDelta
        }
    }).filter(group => group.variants.length > 0)

    const variantCounts = new Map<string, number>()
    variantsByGroup.forEach(group => {
        group.variants.forEach(v => {
            const key = `${group.groupName}-${v.id}`
            variantCounts.set(key, (variantCounts.get(key) || 0) + 1)
        })
    })

    return (
        <Box 
            flex="row" 
            p={4} 
            minW={200} 
            border="1px solid #ccc" 
            borderRadius={4}
            display="flex"
            justifyContent="space-between"
            alignItems="flex-start"
        >
            <Flex direction="column" flex={1} gap={1}>
                <Text fontWeight="bold">{orderItem.orderable.name}</Text>
                {variantsByGroup.map(group => {
                    const consolidatedVariants = new Map<string, { name: string; count: number }>()
                    group.variants.forEach(v => {
                        const existing = consolidatedVariants.get(v.id)
                        if (existing) {
                            existing.count++
                        } else {
                            consolidatedVariants.set(v.id, { name: v.name, count: 1 })
                        }
                    })

                    return (
                        <Text key={group.groupName} color="gray.600" fontSize="sm">
                            {group.groupName}: {Array.from(consolidatedVariants.values())
                                .map(v => v.count > 1 ? `${v.name} ×${v.count}` : v.name)
                                .join(', ')}
                            {group.priceDelta > 0 && (
                                <Text as="span" color="gray.400" ml={2}>
                                    (+${group.priceDelta.toFixed(2)})
                                </Text>
                            )}
                        </Text>
                    )
                })}
            </Flex>
            <Flex align="center" gap={4}>
                <Flex direction="column" align="flex-end">
                    <Text>${totalPrice.toFixed(2)} × {orderItem.quantity}</Text>
                </Flex>
                <Button size="sm" onClick={handleRemove}>Remove</Button>
            </Flex>
        </Box>
    )
}

function VariantGroupSelector({ 
    variantGroup, 
    onSelect, 
    onBack, 
    onNext,
    currentSelections
}: { 
    variantGroup: VariantGroup, 
    onSelect: (variant: Variant) => void,
    onBack: () => void,
    onNext: () => void,
    currentSelections: Variant[]
}) {
    // Only count variants that actually belong to this group
    const selectedVariantsInGroup = currentSelections.filter(selectedVariant => 
        'groupId' in selectedVariant && selectedVariant.groupId === variantGroup.id
    )

    return (
        <Box p={4}>
            <Flex justify="space-between" mb={4}>
                <Box>
                    <Heading size="md">{variantGroup.name}</Heading>
                    {selectedVariantsInGroup.length > 0 && (
                        <Text color="gray.600" fontSize="sm">
                            Selected: {selectedVariantsInGroup.map(v => v.name).join(', ')}
                        </Text>
                    )}
                </Box>
                <Flex gap={2}>
                    <Button size="sm" onClick={onBack}>Back</Button>
                    <Button size="sm" onClick={onNext}>Next</Button>
                </Flex>
            </Flex>
            <Flex direction="column" gap={4}>
                {variantGroup.variants.map((variant) => {
                    const count = selectedVariantsInGroup.filter(v => v.id === variant.id).length
                    return (
                        <Box 
                            key={variant.id}
                            p={4} 
                            border="1px solid #ccc" 
                            borderRadius={4}
                            cursor="pointer"
                            onClick={() => onSelect(variant)}
                        >
                            <Flex justify="space-between" mb={2}>
                                <Flex gap={2} align="center">
                                    <Text fontWeight="bold">{variant.name}</Text>
                                    {count > 0 && (
                                        <Text fontSize="sm">×{count}</Text>
                                    )}
                                </Flex>
                                <Text>${variant.priceDelta > 0 ? `+${variant.priceDelta.toFixed(2)}` : '0.00'}</Text>
                            </Flex>
                        </Box>
                    )
                })}
            </Flex>
        </Box>
    )
}

function VariantSelector({ orderable, setOrder, setInProgressOrder }: { 
    orderable: Orderable, 
    setInProgressOrder: React.Dispatch<React.SetStateAction<Orderable | null>>, 
    setOrder: React.Dispatch<React.SetStateAction<CompleteOrder>> 
}) {
    const [currentGroupIndex, setCurrentGroupIndex] = useState(0)
    const [variantsByGroup, setVariantsByGroup] = useState<Map<string, Variant[]>>(
        new Map(orderable.variantGroups.map(g => [g.id, []]))
    )
    const [quantity, setQuantity] = useState(1)

    const currentGroup = orderable.variantGroups[currentGroupIndex]

    const handleVariantSelect = (variant: Variant) => {
        const selectedVariant: SelectedVariant = {
            ...variant,
            groupId: currentGroup.id
        }
        setVariantsByGroup(prev => {
            const newMap = new Map(prev)
            const currentVariants = [...(prev.get(currentGroup.id) || [])]
            newMap.set(currentGroup.id, [...currentVariants, selectedVariant])
            return newMap
        })
    }

    const handleBack = () => {
        if (currentGroupIndex > 0) {
            setCurrentGroupIndex(currentGroupIndex - 1)
        } else {
            setInProgressOrder(null)
        }
    }

    const handleNext = () => {
        const currentVariants = variantsByGroup.get(currentGroup.id) || []
        if (currentGroup.required && currentVariants.length === 0) {
            return
        }

        if (currentGroupIndex < orderable.variantGroups.length - 1) {
            setCurrentGroupIndex(currentGroupIndex + 1)
        } else {
            const allVariants = Array.from(variantsByGroup.values()).flat() as SelectedVariant[]
            setOrder(prev => ({
                ...prev,
                orderItems: [
                    ...prev.orderItems,
                    {
                        orderable,
                        variants: allVariants,
                        quantity
                    }
                ]
            }))
            setInProgressOrder(null)
        }
    }

    return (
        <Box>
            <Box mb={4}>
                <Heading size="lg">{orderable.name}</Heading>
                <Text color="gray.600">Step {currentGroupIndex + 1} of {orderable.variantGroups.length}</Text>
            </Box>
            <VariantGroupSelector 
                variantGroup={currentGroup}
                onSelect={handleVariantSelect}
                onBack={handleBack}
                onNext={handleNext}
                currentSelections={variantsByGroup.get(currentGroup.id) || []}
            />
            {currentGroupIndex === orderable.variantGroups.length - 1 && (
                <Box mt={4}>
                    <Flex gap={4} align="center" justify="center">
                        <QuantitySelector 
                            setQuantity={setQuantity} 
                            quantity={quantity} 
                        />
                    </Flex>
                </Box>
            )}
        </Box>
    )
}

function AllOrderables({setInProgressOrder}: {setInProgressOrder: React.Dispatch<React.SetStateAction<Orderable | null>>}){
    return (
        <Flex direction={"column"} gap={2}>
        {orderables.map((orderable) => (
            <OrderableCard setInProgressOrder={setInProgressOrder} key={orderable.id} orderable={orderable} />
        ))}
    </Flex>
    )

}

function Order() {
    const [order, setOrder] = useState<CompleteOrder>({
        id: crypto.randomUUID(),
        timestamp: new Date().toISOString(),
        orderItems: []
    })
    const [inProgressOrder, setInProgressOrder] = useState<Orderable | null>(null)

    const orderTotal = order.orderItems.reduce((sum, item) => {
        const itemPrice = item.orderable.price + 
            item.variants.reduce((variantSum, variant) => variantSum + variant.priceDelta, 0)
        return sum + (itemPrice * item.quantity)
    }, 0)

    const handleSubmitOrder = () => {
        console.log('Submitting order:', order)
        setOrder({
            id: crypto.randomUUID(),
            timestamp: new Date().toISOString(),
            orderItems: []
        })
    }

    return (
        <Box p={4}>
            <BreadcrumbComponent />
            {inProgressOrder ? (
                <VariantSelector 
                    orderable={inProgressOrder} 
                    setOrder={setOrder} 
                    setInProgressOrder={setInProgressOrder} 
                />
            ) : (
                <Box>
                    <AllOrderables setInProgressOrder={setInProgressOrder} />
                </Box>
            )}

            {order.orderItems.length > 0 && (
                <Box mt={8}>
                    <Heading size="md" mb={4}>Current Order</Heading>
                    <Flex direction="column" gap={3}>
                        {order.orderItems.map((orderItem, index) => (
                            <InOrderCard 
                                key={`${orderItem.orderable.id}-${orderItem.variants.map(v => v.id).join('-')}-${index}`}
                                setOrder={setOrder} 
                                orderItem={orderItem} 
                            />
                        ))}
                        <Box 
                            p={4} 
                            borderRadius={4}
                            display="flex"
                            justifyContent="space-between"
                        >
                            <Text fontWeight="bold">Total</Text>
                            <Text fontWeight="bold">${orderTotal.toFixed(2)}</Text>
                        </Box>
                        <Button
                            mt={4}
                            colorScheme="green"
                            width="100%"
                            size="lg"
                            onClick={handleSubmitOrder}
                        >
                            Submit Order (${orderTotal.toFixed(2)})
                        </Button>
                    </Flex>
                </Box>
            )}
        </Box>
    )
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
                    <Breadcrumb.CurrentLink>Order</Breadcrumb.CurrentLink>
                </Breadcrumb.Item>
            </Breadcrumb.List>
        </Breadcrumb.Root>
    )
}