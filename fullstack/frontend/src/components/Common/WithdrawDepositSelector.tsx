import {  HStack, Icon, Tag, TagLabel, Box } from "@chakra-ui/react"
import BoxWithText from "./BoxWithText"
import { FiLogIn, FiLogOut } from "react-icons/fi"

export function WithdrawDepositSelector({
  setShowDeposits,
  showDeposits,
}: {
  showDeposits: boolean
  setShowDeposits: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const availableOptions = ["deposits", "withdrawals"]

  return (
    <Box border="1px solid" borderColor="gray.200" borderRadius="md" p={2}>
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
