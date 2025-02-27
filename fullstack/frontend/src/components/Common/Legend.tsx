import { Box, Flex, HStack, Text } from "@chakra-ui/react"
import React from "react"
import { useState } from "react"
import { useColorPalette } from "../../hooks/useColor"
import BoxWithText from "./BoxWithText"

export function Legend({
  toShowNames,
}: { toShowNames: (string | undefined)[] | undefined }) {
  const [isExpanded, setIsExpanded] = useState(true)
  const { getAssignedColors } = useColorPalette()

  const colors = getAssignedColors()

  const toShowColors = Object.entries(colors).filter(([name]) =>
    toShowNames?.includes(name),
  )

  return (
    <div className="w-[300px] ">
      <BoxWithText
        text="Legend"
        isExpanded={isExpanded}
        setIsExpanded={setIsExpanded}
      >
        <Flex margin={3} gap={3} direction="column">
          {toShowColors.map(([name, color], index) => {
            return (
              <LegendItem key={index.toString()} name={name} color={color} />
            )
          })}
        </Flex>
      </BoxWithText>
    </div>
  )
}

export default Legend

export function LegendItem({ name, color }: { name: string; color: string }) {
  return (
    <HStack borderRadius={"md"} borderWidth={1} p={3}>
      <Box
        width="16px"
        borderWidth={1}
        padding={3}
        height="16px"
        borderRadius="50%"
        backgroundColor={color}
      />
      <Text>{name}</Text>
    </HStack>
  )
}
