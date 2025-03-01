import { Box, HStack, Icon, Tag, TagLabel } from "@chakra-ui/react"
import { FiLogIn, FiLogOut } from "react-icons/fi"
import BoxWithText from "./BoxWithText"

export function WithdrawDepositSelector({
  setShowDeposits,
  showDeposits,
}: {
  showDeposits: boolean
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const availableOptions = ["deposits", "withdrawals"]

  return (
    <Box borderWidth={1} borderColor="blue.500" borderRadius="md" p={2}>
      <HStack spaceX={4} wrap="nowrap" align="center">
        {availableOptions.map((option) => {
          const isActive = showDeposits === (option === "deposits")
          return (
            <Tag.Root
              key={option}
              color={!isActive ? "blue.300" : "blue.500"}
              cursor={"pointer"}
              opacity={isActive ? 1 : 0.5}
              p={2}
              borderRadius="md"
              onClick={() =>
                !isActive && setShowDeposits(option === "deposits")
              }
            >
              <Icon as={option === "deposits" ? FiLogIn : FiLogOut} mr={1} />
              <TagLabel>
                {option.charAt(0).toUpperCase() + option.slice(1)}
              </TagLabel>
            </Tag.Root>
          )
        })}
      </HStack>
    </Box>
  )
}
