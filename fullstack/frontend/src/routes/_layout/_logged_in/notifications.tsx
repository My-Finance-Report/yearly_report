import { useQuery } from '@tanstack/react-query'
import { createFileRoute } from '@tanstack/react-router'
import {
    EffectOut,
    NoCodeService
} from "@/client"
import { Card, Container, Text, Flex, Input } from '@chakra-ui/react'

export const Route = createFileRoute('/_layout/_logged_in/notifications')({
    component: NotificationComponent,
})

function NotificationComponent() {


    const { data } = useQuery({
        queryKey: ['effects'],
        queryFn: () => NoCodeService.getEffects()
    }
    )


    return (
        <Container w="100%">
            {data?.map((effect, index) => (
                <ShowEffect key={index} effect={effect} />
            ))}
        </Container>
    )
}


function ShowEffect({ effect }: { effect: EffectOut }) {
    return (
        <Card.Root >
            <Card.Header>
                {effect.name}
            </Card.Header>
            <Card.Body>
                <Flex direction="row" gap={2}>
                    <NotificationCondition effect={effect} />
                    <Text> we will {effect.effect_type} you</Text>
                    <Text>at most once per {effect.config.frequency_days} day(s)</Text>
                </Flex>
                <NotificationPreview subject={effect.config.subject} template={effect.config.template} />
            </Card.Body>
        </Card.Root>
    )
}

function NumberInput({ value }: { value: number }) {
    return <Input variant='subtle' size="sm" type="number" defaultValue={value} />
}


function getStatement(effect: EffectOut) {
    switch (effect.condition) {
        case 'amount_over': {
            const amount = effect.conditional_parameters["amount"] as number
            const comparator = effect.conditional_parameters["comparator"] as string
            return (
                <Flex direction={'row'}>
                    <Text>If a transaction with an amount {comparator}</Text>
                    <NumberInput value={amount} />
                    <Text>was uploaded</Text>
                </Flex>
            )
        }
        case 'count_of_transactions': {
            const count = effect.conditional_parameters["count"] as number
            const comparator = effect.conditional_parameters["comparator"] as string
            return (
                <Flex direction={'row'}>
                    <Text>If {comparator}</Text>
                    <NumberInput value={count} />
                    <Text>transactions are uploaded</Text>
                </Flex>

            )
        }
        default: {
            throw new Error("no implemented")
        }
    }
}

function NotificationCondition({ effect }: { effect: EffectOut }) {

    return (
        getStatement(effect)
    )
}

function NotificationPreview({ subject, template }: { subject: string, template: string }) {

    return (
        <Card.Root>
            <Card.Header>
                {subject}
            </Card.Header>
            <Card.Body>
                {template}
            </Card.Body>
        </Card.Root>

    )
}