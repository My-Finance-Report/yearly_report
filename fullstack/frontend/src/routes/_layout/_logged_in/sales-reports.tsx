import { createFileRoute } from '@tanstack/react-router'
import { Box, Flex, Table, Text, Button, Breadcrumb } from '@chakra-ui/react'
import { useState } from 'react';

export const Route = createFileRoute('/_layout/_logged_in/sales-reports')({
  component: RouteComponent,
})

interface Action {
    name: string;
    getData: () => Promise<any>;
}

const actions: Action[] = [
    {
        name: "Today", 
        getData: () => Promise.resolve({})
    }, 
    {
        name: "This Week", 
        getData: () => Promise.resolve({})
    }, 
    {
        name: "This Month", 
        getData: () => Promise.resolve({})
    },
    {
        name: "This Year", 
        getData: () => Promise.resolve({})
    },
    {
        name: "All Time", 
        getData: () => Promise.resolve({})
    }
    
]

function AllActions({ setData }: { setData: React.Dispatch<React.SetStateAction<any>> }){
    return (
    <Flex direction={"column"} gap={2}>
        {actions.map((action, index) => (
            <ActionCard  key={index} action={action} setData={setData} />
        ))}
    </Flex>
    )

}
function ActionCard({ action, setData }: { action: Action, setData: React.Dispatch<React.SetStateAction<any>> }) {
    return (
    <Box cursor="pointer" minH={100} onClick={() => action.getData().then(data => setData(data))} display={"flex"} flexDirection={"column"} justifyContent="center" alignItems="center" p={2} minW={200} border="1px solid #ccc" borderRadius={4}>
        <Text cursor="pointer">{action.name}</Text>
    </Box>
    )
}


const fakeData = {
    orders: [
        {
            id: "1",
            timestamp: "2022-01-01",
            orderItems: [
                {
                    orderable: {
                        id: "1",
                        name: "ice coffee",
                        variants: [
                            { id: "1", name: "Small", priceDelta: 0 },
                            { id: "2", name: "Medium", priceDelta: 1 },
                            { id: "3", name: "Large", priceDelta: 2 },
                        ],
                        price: 2,
                    },
                    variant: { id: "1", name: "Small", priceDelta: 0 },
                    quantity: 2,
                },
            ],
        },
        {
            id: "2",
            timestamp: "2022-01-02",
            orderItems: [
                {
                    orderable: {
                        id: "1",
                        name: "ice coffee",
                        variants: [
                            { id: "1", name: "Small", priceDelta: 0 },
                            { id: "2", name: "Medium", priceDelta: 1 },
                            { id: "3", name: "Large", priceDelta: 2 },
                        ],
                        price: 2,
                    },
                    variant: { id: "1", name: "Small", priceDelta: 0 },
                    quantity: 1,
                },
            ],
        },
        {
            id: "3",
            timestamp: "2022-01-03",
            orderItems: [
                {
                    orderable: {
                        id: "1",
                        name: "ice coffee",
                        variants: [
                            { id: "1", name: "Small", priceDelta: 0 },
                            { id: "2", name: "Medium", priceDelta: 1 },
                            { id: "3", name: "Large", priceDelta: 2 },
                        ],
                        price: 2,
                    },
                    variant: { id: "1", name: "Small", priceDelta: 0 },
                    quantity: 5,
                },
            ],
        },
    ],
}

function determinePrice(item: any) {
    return (item.orderable.price + item.variant.priceDelta) * item.quantity
}

function formatPrice(price: number) {
    return price.toFixed(2)
}

function DataDisplay({ data }: { data: any }) {
    return (
    <Table.Root>
      <Table.Header>
        <Table.Row>
          <Table.ColumnHeader>Date</Table.ColumnHeader>
          <Table.ColumnHeader>Total</Table.ColumnHeader>
          <Table.ColumnHeader>Actions</Table.ColumnHeader>
        </Table.Row>
      </Table.Header>
      {fakeData.orders.map((order: any) => (
        <Table.Row key={order.id}>
          <Table.Cell>{order.timestamp}</Table.Cell>
          <Table.Cell>${formatPrice(order.orderItems.reduce((sum: number, item: any) => sum + determinePrice(item), 0))}</Table.Cell>
          <Table.Cell>
            <Button size="sm">View</Button>
          </Table.Cell>
        </Table.Row>
      ))}
    </Table.Root>
    )
}

function RouteComponent() {
    const [data, setData] = useState<any>(null)
    return (
    <>
    <BreadcrumbComponent />
    {data ? <DataDisplay data={data}/> : <AllActions setData={setData}/>}
    </>
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
                    <Breadcrumb.CurrentLink>Sales Reports</Breadcrumb.CurrentLink>
                </Breadcrumb.Item>
            </Breadcrumb.List>
        </Breadcrumb.Root>
    )
}