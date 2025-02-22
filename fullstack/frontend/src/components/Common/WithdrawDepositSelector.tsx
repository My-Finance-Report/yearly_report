import { AddIcon } from "@chakra-ui/icons"
import { FiLogIn, FiLogOut } from "react-icons/fi"
import {
    Box,
    Tag,
    Text,
    HStack,
    Icon,
    TagLabel,
} from "@chakra-ui/react"

export function WithdrawDepositSelector({ setShowDeposits, showDeposits }: { showDeposits: boolean, setShowDeposits: React.Dispatch<React.SetStateAction<boolean>> }) {

    const availableOptions = ["deposits", "withdrawals"]

    return (
        <Box p={4} borderWidth={1} borderRadius="md">
            <HStack spaceX={4} wrap="nowrap" align="center">
                {availableOptions.map((option) => {
                    const isActive = showDeposits === (option === "deposits")
                    return (
                        <Tag.Root
                            key={option}
                            cursor={"pointer"}
                            opacity={!isActive ? 1 : 0.5}
                            p={2}
                            borderRadius="md"
                            onClick={() => !isActive && setShowDeposits(option === "deposits")}
                        >
                            <Icon as={option === "deposits" ? FiLogIn : FiLogOut} mr={1} />
                            <TagLabel>
                                {option.charAt(0).toUpperCase() + option.slice(1)}
                            </TagLabel>
                        </Tag.Root>
                    )
                })}

                <Box borderWidth={1} minH={10} borderRight="3px solid" borderColor="gray.300" />

                <Text>Showing</Text>
                <Tag.Root paddingY={1.5} paddingX={2} size="lg" cursor="default">
                    <Text>
                        {showDeposits ? "Deposits" : "Withdrawals"}
                    </Text>
                </Tag.Root>
            </HStack>
        </Box>
    )
}
