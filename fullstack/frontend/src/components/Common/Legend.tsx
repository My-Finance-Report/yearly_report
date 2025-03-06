import { Box, Flex, HStack, Text } from "@chakra-ui/react"
import type React from "react"
import { FiMaximize2 } from "react-icons/fi"
import { useColorPalette } from "../../hooks/useColor"
import BoxWithText, { type CollapsibleName, NAME_TO_ICON } from "./BoxWithText"

export function Legend({
  toShowNames,
  collapsedItems,
  setCollapsedItems,
}: {
  toShowNames: (string | undefined)[] | undefined
  collapsedItems: CollapsibleName[]
  setCollapsedItems: React.Dispatch<React.SetStateAction<CollapsibleName[]>>
}) {
  const { getAssignedColors } = useColorPalette()

  const colors = getAssignedColors()

  const toShowColors = Object.entries(colors).filter(([name]) =>
    toShowNames?.includes(name),
  )

  const showColors = false

  return (
    <div className="w-[200px]" style={{ position: "sticky", top: 80 }}>
      {showColors && (
        <div style={{ paddingTop: "10px" }}>
          <BoxWithText text="Legend" isCollapsable={false}>
            <Flex margin={3} gap={3} direction="column">
              {toShowColors.map(([name, color], index) => {
                return (
                  <LegendItem
                    key={index.toString()}
                    name={name}
                    color={color}
                  />
                )
              })}
            </Flex>
          </BoxWithText>
        </div>
      )}
      {collapsedItems.length > 0 && (
        <div style={{ paddingTop: "20px" }}>
          <BoxWithText
            text="Collapsed"
            collapsedItems={collapsedItems}
            setCollapsedItems={setCollapsedItems}
            isCollapsable={false}
          >
            <Flex margin={3} gap={3} direction="column">
              {collapsedItems.map((name, index) => {
                return (
                  <CollapsedWidget
                    key={index.toString()}
                    name={name}
                    onClick={() =>
                      setCollapsedItems(
                        collapsedItems.filter((item) => item !== name),
                      )
                    }
                  />
                )
              })}
            </Flex>
          </BoxWithText>
        </div>
      )}
    </div>
  )
}

export default Legend

function CollapsedWidget({
  name,
  onClick,
}: { name: CollapsibleName; onClick: () => void }) {
  const icon = NAME_TO_ICON[name]
  return (
    <HStack
      borderRadius={"md"}
      borderWidth={1}
      p={3}
      cursor="pointer"
      onClick={onClick}
      justify="space-between"
    >
      {icon}
      <Text>{name}</Text>
      <FiMaximize2 />
    </HStack>
  )
}

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
