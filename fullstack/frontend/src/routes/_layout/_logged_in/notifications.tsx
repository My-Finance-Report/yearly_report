import { useQuery } from '@tanstack/react-query'
import { createFileRoute } from '@tanstack/react-router'
import {
    EffectOut,
    NoCodeService
} from "@/client"
import { 
    Container, Text, Flex, Input, Box, Heading, VStack, HStack, Badge, 
    Icon, Spacer, Button, Tooltip,
    Separator
} from '@chakra-ui/react'
import { FaBell, FaEnvelope, FaEdit, FaToggleOn } from 'react-icons/fa'

export const Route = createFileRoute('/_layout/_logged_in/notifications')({
    component: NotificationComponent,
})

function NotificationComponent() {
    const { data, isLoading } = useQuery({
        queryKey: ['effects'],
        queryFn: () => NoCodeService.getEffects()
    })


    return (
        <Container maxW="container.lg" py={8}>
            <Box mb={8} p={6}  borderRadius="lg" shadow="md">
                <Flex align="center" mb={4}>
                    <Icon as={FaBell} fontSize="2xl" color="blue.500" mr={3} />
                    <Heading size="lg">Notification Settings</Heading>
                    <Spacer />
                    <Button colorScheme="blue"  size="sm">
                    <FaEdit/>
                        Add New
                    </Button>
                </Flex>
                <Text color="gray.600">
                    Configure how and when you want to be notified about your financial activities.
                </Text>
            </Box>

            {isLoading ? (
                <Flex justify="center" p={8}>
                    <Text>Loading your notification settings...</Text>
                </Flex>
            ) : (
                <VStack spaceX={6} align="stretch">
                    {data?.map((effect, index) => (
                        <ShowEffect key={index} effect={effect} />
                    ))}
                </VStack>
            )}
        </Container>
    )
}


function ShowEffect({ effect }: { effect: EffectOut }) {
   
    return (
        <Box 
            borderWidth="1px" 
            borderRadius="lg" 
            shadow="md"
            overflow="hidden"
        >
            <Flex 
                p={4} 
                borderBottomWidth="1px"
                align="center"
            >
                <Icon 
                    as={effect.effect_type === 'email' ? FaEnvelope : FaBell} 
                    color={effect.effect_type === 'email' ? 'blue.500' : 'purple.500'} 
                    mr={3} 
                    fontSize="xl"
                />
                <Heading size="md">{effect.name}</Heading>
                <Spacer />
                <Badge 
                    colorScheme="green" 
                    variant="subtle" 
                    px={2} 
                    py={1} 
                    borderRadius="full"
                >
                    Active
                </Badge>
                <Tooltip.Root>
                    <Button variant="ghost" size="sm" ml={2}>
                        <Icon as={FaToggleOn} color="green.500" fontSize="lg" />
                    </Button>
                </Tooltip.Root>
            </Flex>
            
            <Box p={5}>
                <VStack align="stretch" spaceX={4}>
                    <Box>
                        <Flex 
                            p={3} 
                            borderRadius="md"
                            direction="column"
                            gap={2}
                        >
                            <Text fontWeight="medium" fontSize="sm" color="gray.600">
                                Trigger Condition:
                            </Text>
                            <NotificationCondition effect={effect} />
                            
                            <Separator my={2} />
                            
                            <HStack spaceY={3}>
                                <Badge colorScheme="blue" variant="subtle" px={2} py={1}>
                                    {effect.effect_type === 'email' ? 'Email Notification' : 'In-app Notification'}
                                </Badge>
                                <Text fontSize="sm" color="gray.600">
                                    Frequency: At most once per {effect.config.frequency_days} day(s)
                                </Text>
                            </HStack>
                        </Flex>
                    </Box>
                    
                    <Box mt={2}>
                        <Text fontWeight="medium" mb={2} color="gray.600">
                            Preview:
                        </Text>
                        <NotificationPreview subject={effect.config.subject} template={effect.config.template} />
                    </Box>
                </VStack>
            </Box>
            
            <Flex 
                p={3} 
                borderTopWidth="1px"
                justify="flex-end"
            >
                <Button size="sm" colorScheme="blue" variant="outline" mr={2}>
                    Edit
                </Button>
                <Button size="sm" colorScheme="red" variant="outline">
                    Delete
                </Button>
            </Flex>
        </Box>
    )
}

function NumberInput({ value }: { value: number }) {
    return (
        <Input 
            size="sm" 
            type="number" 
            defaultValue={value} 
            width="70px" 
            mx={2}
            textAlign="center"
            fontWeight="medium"
            _hover={{ borderColor: 'blue.400' }}
        />
    )
}


function getStatement(effect: EffectOut) {
    
    switch (effect.condition) {
        case 'amount_over': {
            const amount = effect.conditional_parameters["amount"] as number
            const comparator = effect.conditional_parameters["comparator"] as string
            return (
                <Flex 
                    direction={'row'} 
                    align="center" 
                    wrap="wrap"
                    fontWeight="medium"
                >
                    <Text color="gray.600">If a transaction with an amount</Text>
                    <Text mx={1} fontWeight="bold" color="blue.500">{comparator}</Text>
                    <NumberInput value={amount} />
                    <Text color="gray.600">was uploaded</Text>
                </Flex>
            )
        }
        case 'count_of_transactions': {
            const count = effect.conditional_parameters["count"] as number
            const comparator = effect.conditional_parameters["comparator"] as string
            return (
                <Flex 
                    direction={'row'} 
                    align="center" 
                    wrap="wrap"
                    fontWeight="medium"
                >
                    <Text color="gray.600">If</Text>
                    <Text mx={1} fontWeight="bold" color="blue.500">{comparator}</Text>
                    <NumberInput value={count} />
                    <Text color="gray.600">transactions are uploaded</Text>
                </Flex>
            )
        }
        default: {
            throw new Error("not implemented")
        }
    }
}

function NotificationCondition({ effect }: { effect: EffectOut }) {
    return getStatement(effect)
}

function NotificationPreview({ subject, template }: { subject: string, template: string }) {
   
    return (
        <Box 
            borderWidth="1px" 
            borderRadius="md" 
            overflow="hidden"
        >
            <Box 
                p={3} 
                borderBottomWidth="1px"
                fontWeight="medium"
            >
                {subject}
            </Box>
            <Box p={4}>
                {template}
            </Box>
        </Box>
    )
}