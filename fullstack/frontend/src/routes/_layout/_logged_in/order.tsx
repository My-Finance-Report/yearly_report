import { createFileRoute } from '@tanstack/react-router'
import { Box, Flex, Text, Button, Heading, Input, NumberInput } from '@chakra-ui/react'
import { useState } from 'react';

interface Variant {
    id: string;
    name: string;
    priceDelta: number;
}

interface Orderable {
    id: string;
    name: string;
    variants: Variant[];
    price: number;
}


export const Route = createFileRoute('/_layout/_logged_in/order')({
  component: Order,
})


const orderables: Orderable[] = [
    {
        id: "1",
        name: "T-shirt",
        variants: [
            { id: "1", name: "Small", priceDelta: 0 },
            { id: "2", name: "Medium", priceDelta: 1 },
            { id: "3", name: "Large", priceDelta: 2 },
        ],
        price: 10,
    },
    {
        id: "2",
        name: "Jeans",
        variants: [
            { id: "1", name: "Small", priceDelta: 0 },
            { id: "2", name: "Medium", priceDelta: 1 },
            { id: "3", name: "Large", priceDelta: 2 },
        ],
        price: 20,
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

function OrderableCard({ setOrder, orderable }: { setOrder: React.Dispatch<React.SetStateAction<Orderable[]>>, orderable: Orderable }) {
    const [showVariants, setShowVariants] = useState(false)
    const [quantity, setQuantity] = useState(1)
    return (<Box onClick={() => setShowVariants(!showVariants)} flex={"row"} p={2} minW={200} border="1px solid #ccc" borderRadius={4}>
        <Text>{orderable.name}</Text>
        {showVariants && (
            <Flex direction={"column"} gap={2}>
                {orderable.variants.map((variant) => (
                    <Box key={variant.id} p={2} border="1px solid #ccc" borderRadius={4}>
                        <Text>{variant.name}</Text>
                        <Text>${orderable.price + variant.priceDelta}</Text>
                        <Flex direction={"row"} gap={2}>
                        <QuantitySelector quantity={quantity} setQuantity={setQuantity} />
                        <Button onClick={() => setOrder((prev) => [...prev, { ...orderable, variant }])}>Add to Order</Button>
</Flex>
                    </Box>
                ))}
            </Flex>
        )}
    </Box>)
}

function InOrderCard({ setOrder, orderable }: { setOrder: React.Dispatch<React.SetStateAction<Orderable[]>>, orderable: Orderable }) {
    return (<Box flex={"row"} p={2} minW={200} border="1px solid #ccc" borderRadius={4}>
        <Text>{orderable.name}</Text>
        <Text>{orderable.variants.find((variant) => variant.id === orderable.variants?.id)?.name}</Text>
        <Text>{orderable.variants.find((variant) => variant.id === orderable.variants?.id)?.priceDelta}</Text>
        <Button onClick={() => setOrder((prev) => prev.filter((item) => item.id !== orderable.id))}>Remove from Order</Button>
    </Box>)
}
function Order() {
    const [order, setOrder] = useState<Orderable[]>([])
    return (<Box p={2}>
    <Flex direction={"column"} gap={2}>
        {orderables.map((orderable) => (
            <OrderableCard setOrder={setOrder} key={orderable.id} orderable={orderable} />
        ))}
    </Flex>
    <Heading size="md">Order</Heading>
    <Flex direction={"column"} gap={2}>
        {order.map((orderable) => (
            <InOrderCard setOrder={setOrder} key={orderable.id} orderable={orderable} />
        ))}
    </Flex>
  </Box>)
}

