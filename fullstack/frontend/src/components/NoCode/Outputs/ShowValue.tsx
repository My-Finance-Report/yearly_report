import { NoCodeWidget } from "@/client"
import { Text, Flex } from "@chakra-ui/react"

export function ShowValue({ widget }: { widget: NoCodeWidget }) {
    return (
        <Flex direction="column">
        <Text>{widget.name}</Text>
        <Text>{widget.result.result.value}</Text>
</Flex>
    )
}
