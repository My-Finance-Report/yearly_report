import { useState } from "react"
import { createListCollection } from "@chakra-ui/react"
import "reactflow/dist/style.css"
import {
    Box,
    Button,
    Flex,
    VStack,
    Text,
    Spinner,
    HStack,
} from "@chakra-ui/react"

import {
    SelectRoot,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValueText,
} from "@/components/ui/select"

import { useMutation, useQuery } from "@tanstack/react-query"
import {
    type SankeyInputCreate,
    type SankeyLinkageCreate,
    SankeyService,
} from "../../client"
import { isLoggedIn } from "@/hooks/useAuth"

type blah = { label: string, value: string | number }

export function SankeyConfigPage() {
    const [selectedInputs, setSelectedInputs] = useState<blah[]>([])
    const [selectedLinkages, setSelectedLinkages] = useState<blah[]>([])
    const [selectedInput, setSelectedInput] = useState<blah | null>(null)
    const [selectedLinkage, setSelectedLinkage] = useState<blah | null>(null)

    const { data, isLoading } = useQuery({
        queryKey: ["sankeyData"],
        queryFn: async () => {
            return SankeyService.getSankeyConfigInfo()
        },
        enabled: isLoggedIn(),
    })

    const makeKeyFromLink = (link: SankeyLinkageCreate) => (
        `${link.category_id}+${link.target_source_id}`
    )
    const parseLinkFromKey = (key: blah) => {
        const [category_idStr, source_idStr] = key.value.toString().split("+")
        const category_id = Number(category_idStr)
        const target_source_id = Number(source_idStr)
        return {
            category_id,
            target_source_id,
        }
    }


    const selectedCategoryNames = new Set(selectedInputs.map((input) => input.value))

    const collectionOfInputs = {
        items:
            data?.possible_inputs?.map((input) => ({
                label: input.category_name,
                value: input.category_id,
            })) || [],
    }

    const collectionOfLinkages = {
        items:
            data?.possible_links
                ?.filter((link) => selectedCategoryNames.has(link.category_id))
                .map((link) => ({
                    label: `${link.category_name} -> ${link.target_source_name}`,
                    value: makeKeyFromLink(link),
                })) || [],
    }

    const addInput = () => {
        if (
            selectedInput &&
            !selectedInputs.some((i) => i.value === selectedInput.value)
        ) {

            setSelectedInputs([...selectedInputs, selectedInput])
            setSelectedInput(null)
        }
    }


    const addLinkage = () => {
        if (
            selectedLinkage &&
            !selectedLinkages.some(
                (l) => l.value === selectedLinkage.value
            )
        ) {
            setSelectedLinkages([...selectedLinkages, selectedLinkage])
            setSelectedLinkage(null)
        }
    }

    const removeInput = (inputToRemove: blah) => {
        setSelectedInputs(
            selectedInputs.filter(
                (input) => input.value !== inputToRemove.value,
            ),
        )
    }

    const removeLinkage = (linkToRemove: blah) => {
        setSelectedLinkages(
            selectedLinkages.filter(
                (link) => !(link.value === linkToRemove.value)
            ),
        )
    }

    const saveSankeyConfig = useMutation({
        mutationFn: async () => {
            const sankeyConfig = {
                requestBody: {
                    inputs: selectedInputs.map((input) => ({
                        category_id: Number(input.value),
                    })),
                    links: selectedLinkages.map((link) => parseLinkFromKey(link)),
                }
            }
            return SankeyService.createSankeyConfig(sankeyConfig)
        },
        onSuccess: () => {
            alert("Sankey Configuration Saved!")
        },
        onError: () => {
            alert("Error saving configuration")
        },
    })

    return (
        <Flex direction="column" p={4} h="100vh">
            <VStack align="start" spaceX={4} width="100%">
                <Box>
                    <Text fontWeight="bold">Selected Inputs:</Text>
                    <HStack wrap="wrap">
                        {selectedInputs.map((input) => (
                            <HStack
                                key={input.label}
                                px={2}
                                py={1}
                                borderRadius="md"
                                spaceX={2}
                            >
                                <Text>{input.label}</Text>
                                <Button size="xs" onClick={() => removeInput(input)}>
                                    Remove
                                </Button>
                            </HStack>
                        ))}
                    </HStack>
                </Box>

                <Box>
                    <Text fontWeight="bold">Selected Linkages:</Text>
                    <HStack wrap="wrap">
                        {selectedLinkages.map((link) => (
                            <HStack
                                key={link.value}
                                px={2}
                                py={1}
                                borderRadius="md"
                                spaceX={2}
                            >
                                <Text>
                                    {link.label}
                                </Text>
                                <Button size="xs" onClick={() => removeLinkage(link)}>
                                    Remove
                                </Button>
                            </HStack>
                        ))}
                    </HStack>
                </Box>

                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                onValueChange={(selectedItems) => {
                                    setSelectedInput(selectedItems.items[0])
                                    console.log(selectedInput)
                                }
                                }
                                collection={createListCollection(collectionOfInputs)}
                                size="sm"
                                width="320px"
                            >
                                <SelectTrigger>
                                    <SelectValueText placeholder="Select an input" />
                                </SelectTrigger>
                                <SelectContent>
                                    {collectionOfInputs.items.map((input) => (
                                        <SelectItem item={input} key={input.value}>
                                            {input.label}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </SelectRoot>

                            <Button onClick={addInput} disabled={!selectedInput}>
                                Add
                            </Button>
                        </HStack>
                    )}
                </Box>

                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                onValueChange={(input) => {
                                    setSelectedLinkage(input.items[0])
                                }}
                                collection={createListCollection(collectionOfLinkages)}
                                size="sm"
                                width="320px"
                            >
                                <SelectTrigger>
                                    <SelectValueText placeholder="Select a linkage" />
                                </SelectTrigger>
                                <SelectContent>
                                    {collectionOfLinkages.items.map((link) => (
                                        <SelectItem
                                            item={link}
                                            key={link.value}
                                        >
                                            {link.label}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </SelectRoot>

                            <Button onClick={addLinkage} disabled={!selectedLinkage}>
                                Add
                            </Button>
                        </HStack>
                    )}
                </Box>

                <Button
                    colorScheme="blue"
                    onClick={() => saveSankeyConfig.mutate()}
                    disabled={selectedInputs.length === 0 && selectedLinkages.length === 0}
                >
                    Save Configuration
                </Button>
            </VStack>
        </Flex >
    )
}
