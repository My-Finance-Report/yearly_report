import type { TransactionSourceGroup } from "@/client"
import { Box, Tag, Text, HStack } from "@chakra-ui/react"
import type React from "react"

export function TransactionSourceSelector({
    allTransactionSources,
    activeTransactionSource,
    setActiveTransactionSource
}: {
    allTransactionSources: TransactionSourceGroup[]
    activeTransactionSource: TransactionSourceGroup,
    setActiveTransactionSource: React.Dispatch<React.SetStateAction<TransactionSourceGroup | null>>,
}) {

    return (
        <Box p={4} borderWidth={1} borderRadius="md">
            <HStack spaceX={4} wrap="nowrap" align="center" >
                {allTransactionSources.map((sourceGroup, index) => {
                    const isActive = activeTransactionSource.transaction_source_id === sourceGroup.transaction_source_id
                    return (
                        <Tag.Root
                            key={index.toString()}
                            cursor="pointer"
                            opacity={!isActive ? 1 : 0.5}
                            p={2}
                            borderRadius="md"
                            onClick={() => { setActiveTransactionSource(sourceGroup) }}
                        >
                            {sourceGroup.transaction_source_name}
                        </Tag.Root>
                    )
                })}

                <Box borderWidth={1} minH={10} borderRight="3px solid" borderColor="gray.300" />

                <Text>From</Text>
                <Tag.Root paddingY={1.5} paddingX={2} size="lg" cursor="default">
                    <Text>
                        {activeTransactionSource.transaction_source_name}
                    </Text>
                </Tag.Root>

            </HStack>
        </Box>
    )
}
