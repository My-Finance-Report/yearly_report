import { useState, useEffect } from "react"
import { createListCollection } from "@chakra-ui/react"
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

/**
 * A generic type for the "item" shape we store in local state
 * (selectedInputs, selectedLinkages). We store a string/number
 * for value, plus a label for display.
 */
type Blah = { label: string; value: string | number }

export function SankeyConfigPage() {
    // Local state for "currently selected" items
    const [selectedInputs, setSelectedInputs] = useState<Blah[]>([])
    const [selectedLinkages, setSelectedLinkages] = useState<Blah[]>([])

    // Local state for "active pick" from each select
    const [selectedInput, setSelectedInput] = useState<Blah | null>(null)
    const [selectedLinkage, setSelectedLinkage] = useState<Blah | null>(null)

    // Fetch from server
    const { data, isLoading } = useQuery({
        queryKey: ["sankeyData"],
        queryFn: SankeyService.getSankeyConfigInfo,
        enabled: isLoggedIn(),
    })

    /**
     * Convert a SankeyLinkageCreate to a unique string key, e.g. "categoryId+targetSourceId"
     */
    const makeKeyFromLink = (link: SankeyLinkageCreate) => {
        return `${link.category_id}+${link.target_source_id}`
    }

    /**
     * Parse that string key back into { category_id, target_source_id }
     */
    const parseLinkFromKey = (item: Blah) => {
        const [categoryIdStr, sourceIdStr] = item.value.toString().split("+")
        return {
            category_id: Number(categoryIdStr),
            target_source_id: Number(sourceIdStr),
        }
    }

    /**
     * 1) On initial load (once data is fetched),
     *    populate selectedInputs and selectedLinkages from the server’s `existing_*`.
     * 2) If you only want to do this once, you can add a guard so that you don’t overwrite user picks
     *    (e.g., track that you’ve done the init with a `didInit` boolean).
     */
    useEffect(() => {
        if (!isLoading && data) {
            // existing_inputs => transform to Blah[] with { label, value }
            const initInputs: Blah[] =
                data.existing_inputs?.map((inp) => ({
                    label: inp.category_name,
                    value: inp.category_id,
                })) || []

            // existing_links => transform to Blah[] with { label, value: "categoryId+targetSourceId" }
            const initLinks: Blah[] =
                data.existing_links?.map((lk) => ({
                    label: `${lk.category_name} -> ${lk.target_source_name}`,
                    value: `${lk.category_id}+${lk.target_source_id}`,
                })) || []

            setSelectedInputs(initInputs)
            setSelectedLinkages(initLinks)
        }
    }, [data, isLoading])

    // Derive which categories have already been selected
    const selectedCategoryIds = new Set(
        selectedInputs.map((input) => input.value),
    )

    // Build "possible inputs" for the select
    const collectionOfInputs = {
        items:
            data?.possible_inputs?.map((input) => ({
                label: input.category_name,
                value: input.category_id,
            })) || [],
    }

    // Build "possible linkages" for the select,
    // filtered by which categories are selected.
    const collectionOfLinkages = {
        items:
            data?.possible_links
                ?.filter((link) => selectedCategoryIds.has(link.category_id))
                .map((link) => ({
                    label: `${link.category_name} -> ${link.target_source_name}`,
                    value: makeKeyFromLink(link),
                })) || [],
    }

    // Handlers
    const addInput = () => {
        if (selectedInput && !selectedInputs.some((i) => i.value === selectedInput.value)) {
            setSelectedInputs((prev) => [...prev, selectedInput])
            setSelectedInput(null)
        }
    }

    const addLinkage = () => {
        if (selectedLinkage && !selectedLinkages.some((l) => l.value === selectedLinkage.value)) {
            setSelectedLinkages((prev) => [...prev, selectedLinkage])
            setSelectedLinkage(null)
        }
    }

    const removeInput = (inputToRemove: Blah) => {
        setSelectedInputs((prev) => prev.filter((input) => input.value !== inputToRemove.value))
    }

    const removeLinkage = (linkToRemove: Blah) => {
        setSelectedLinkages((prev) =>
            prev.filter((link) => link.value !== linkToRemove.value),
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
                },
            }
            return SankeyService.createSankeyConfig(sankeyConfig)
        },
        onSuccess: () => {
            alert("Sankey Configuration Saved!")
        },
        onError: (err) => {
            console.error(err)
            alert("Error saving configuration")
        },
    })

    return (
        <Flex direction="column" p={4} h="100vh">
            <VStack align="start" spaceX={4} width="100%">
                {/* Selected Inputs */}
                <Box>
                    <Text fontWeight="bold">Selected Inputs:</Text>
                    <HStack wrap="wrap">
                        {selectedInputs.map((input) => (
                            <HStack
                                key={input.value}
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

                {/* Selected Linkages */}
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
                                <Text>{link.label}</Text>
                                <Button size="xs" onClick={() => removeLinkage(link)}>
                                    Remove
                                </Button>
                            </HStack>
                        ))}
                    </HStack>
                </Box>

                {/* Input Select */}
                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                onValueChange={(selectedItems) => {
                                    // In single-select, we only have one item
                                    if (selectedItems.items[0]) {
                                        setSelectedInput(selectedItems.items[0])
                                    }
                                }}
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

                {/* Linkage Select */}
                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                onValueChange={(selectedItems) => {
                                    if (selectedItems.items[0]) {
                                        setSelectedLinkage(selectedItems.items[0])
                                    }
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
                                        <SelectItem item={link} key={link.value}>
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

                {/* Save */}
                <Button
                    colorScheme="blue"
                    onClick={() => saveSankeyConfig.mutate()}
                    disabled={selectedInputs.length === 0 && selectedLinkages.length === 0}
                >
                    Save Configuration
                </Button>
            </VStack>
        </Flex>
    )
}
