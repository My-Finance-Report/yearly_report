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
    SelectRoot,
    SelectTrigger,
    SelectValueText,
    SelectContent,
    SelectItem,
    type SelectValueChangeDetails,
    Tag,
    TagLabel,
    HStack,
} from "@chakra-ui/react"
import { useMutation, useQuery } from "@tanstack/react-query"
import { type SankeyInputCreate, type SankeyLinkageCreate, SankeyService } from "../../client"
import { isLoggedIn } from "@/hooks/useAuth"

export function SankeyConfigPage() {
    const [selectedInputs, setSelectedInputs] = useState<SankeyInputCreate[]>([])
    const [selectedLinkages, setSelectedLinkages] = useState<SankeyLinkageCreate[]>([])
    const [selectedInput, setSelectedInput] = useState<SankeyInputCreate | null>(null)
    const [selectedLinkage, setSelectedLinkage] = useState<SankeyLinkageCreate | null>(null)

    console.log(selectedInput)

    const { data, isLoading } = useQuery({
        queryKey: ["sankeyData"],
        queryFn: async () => {
            return SankeyService.getSankeyConfigInfo()
        },
        enabled: isLoggedIn(),
    })
    const selectedCategoryIds = new Set(selectedInputs.map(input => input.category_id))

    const collectionOfInputs = {
        items: data?.possible_inputs?.map((input) => ({ label: input.category_name, value: input })) || []
    }

    const collectionOfLinkages = {
        items: data?.possible_links
            ?.filter(link => selectedCategoryIds.has(link.category_id))
            .map((link) => ({
                label: `${link.category_name} -> ${link.source_name}`,
                value: link
            })) || []
    }

    const addInput = () => {
        if (selectedInput && !selectedInputs.some((i) => i.category_name === selectedInput.category_name)) {
            setSelectedInputs([...selectedInputs, selectedInput])
            setSelectedInput(null)
        }
    }

    const addLinkage = () => {
        if (selectedLinkage && !selectedLinkages.some((l) => l.source_name === selectedLinkage.source_name && l.category_name === selectedLinkage.category_name)) {
            setSelectedLinkages([...selectedLinkages, selectedLinkage])
            setSelectedLinkage(null)
        }
    }

    const removeInput = (inputToRemove: SankeyInputCreate) => {
        setSelectedInputs(selectedInputs.filter((input) => input.category_name !== inputToRemove.category_name))
    }

    const removeLinkage = (linkToRemove: SankeyLinkageCreate) => {
        setSelectedLinkages(selectedLinkages.filter((link) => !(link.source_name === linkToRemove.source_name && link.category_name === linkToRemove.category_name)))
    }

    const saveSankeyConfig = useMutation({
        mutationFn: async () => {
            const sankeyConfig = {
                inputs: selectedInputs.map((input) => ({ category_name: input.category_name })),
                linkages: selectedLinkages.map((link) => ({
                    source_name: link.source_name,
                    category_name: link.category_name,
                })),
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
                            <Tag.Root key={input.category_name} size="lg" variant="solid" colorScheme="blue">
                                <TagLabel>{input.category_name}</TagLabel>
                                <Button onClick={() => removeInput(input)} />
                            </Tag.Root>
                        ))}
                    </HStack>
                </Box>

                <Box>
                    <Text fontWeight="bold">Selected Linkages:</Text>
                    <HStack wrap="wrap">
                        {selectedLinkages.map((link) => (
                            <Tag.Root key={`${link.source_name}-${link.category_name}`} size="lg" variant="solid" colorScheme="green">
                                <TagLabel>{link.source_name} - {link.category_name}</TagLabel>
                                <Button onClick={() => removeLinkage(link)} />
                            </Tag.Root>
                        ))}
                    </HStack>
                </Box>

                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                onValueChange={(e) => setSelectedInput(e.value[0] as unknown as SankeyInputCreate)}
                                collection={createListCollection(collectionOfInputs)}
                                size="sm"
                                width="320px"
                            >
                                <SelectTrigger>
                                    <SelectValueText placeholder="Select an input" />
                                </SelectTrigger>
                                <SelectContent>
                                    {collectionOfInputs.items.map((input) => (
                                        <SelectItem item={input} key={input.value.category_name}>
                                            {input.label}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </SelectRoot>
                            <Button onClick={addInput} disabled={!selectedInput}>Add</Button>
                        </HStack>
                    )}
                </Box>

                <Box>
                    {isLoading ? (
                        <Spinner />
                    ) : (
                        <HStack>
                            <SelectRoot
                                value={selectedInput ? [selectedInput] : undefined}
                                onValueChange={(e: SelectValueChangeDetails<{
                                    label: string;
                                    value: SankeyLinkageCreate;
                                }>) => setSelectedInput(e.value[0] as unknown as SankeyInputCreate)}
                                collection={createListCollection(collectionOfLinkages)}
                                size="sm"
                                width="320px"
                            >
                                <SelectTrigger>
                                    <SelectValueText placeholder="Select a linkage" />
                                </SelectTrigger>
                                <SelectContent>
                                    {collectionOfLinkages.items.map((link) => (
                                        <SelectItem item={link} key={`${link.value.source_name}-${link.value.category_name}`}>
                                            {link.label}
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </SelectRoot>
                            <Button onClick={addLinkage} disabled={!selectedLinkage}>Add</Button>
                        </HStack>
                    )}
                </Box>

                <Button colorScheme="blue" onClick={() => saveSankeyConfig.mutate()} disabled={selectedInputs.length === 0 && selectedLinkages.length === 0}>
                    Save Configuration
                </Button>
            </VStack>
        </Flex>
    )
}
