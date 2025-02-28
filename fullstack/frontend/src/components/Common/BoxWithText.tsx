import { Box, Button, Text } from "@chakra-ui/react"
import type React from "react"
import {
  FiArrowRightCircle,
  FiBarChart,
  FiFilter,
  FiMinimize2,
  FiPieChart,
} from "react-icons/fi"

export type CollapsibleName = keyof typeof NAME_TO_ICON

export const NAME_TO_ICON = {
  "Flow Chart": <FiArrowRightCircle />,
  "Bar Chart": <FiBarChart />,
  "Pie Chart": <FiPieChart />,
  Filters: <FiFilter />,
}

interface LabeledBoxProps {
  COMPONENT_NAME?: CollapsibleName
  text: string
  position?: "top" | "bottom" | "left" | "right"
  children: React.ReactNode
  minH?: number | string
  maxH?: number | string
  minW?: number | string
  maxW?: number | string
  isCollapsable?: boolean
  containerRef?: React.RefObject<HTMLDivElement>
  width?: number | string
  setCollapsedItems?:
    | React.Dispatch<React.SetStateAction<CollapsibleName[]>>
    | undefined
  collapsedItems?: CollapsibleName[]
}

export default function LabeledBox({
  COMPONENT_NAME,
  text,
  position = "top",
  children,
  collapsedItems,
  setCollapsedItems,
  isCollapsable = true,
  minH,
  maxH,
  width,
  maxW,
  minW = "50%",
  containerRef,
}: LabeledBoxProps) {
  const labelStyles = {
    position: "absolute",
    backgroundColor: "black",
    color: "white",
    px: 2,
    fontSize: "sm",
    whiteSpace: "nowrap",
  }

  const positionStyles = {
    top: {
      top: -3,
      left: "50%",
      transform: "translateX(-50%)",
    },
    bottom: {
      bottom: -3,
      left: "50%",
      transform: "translateX(-50%)",
    },
    left: {
      left: -3,
      top: "50%",
      transform: "translateY(-50%) rotate(-90deg)",
    },
    right: {
      right: -3,
      top: "50%",
      transform: "translateY(-50%) rotate(90deg)",
    },
  }

  const isCollapsed = COMPONENT_NAME && collapsedItems?.includes(COMPONENT_NAME)

  if (isCollapsed) {
    return null
  }

  return (
    <Box
      width={width}
      minH={minH}
      maxH={maxH}
      maxW={maxW}
      minW={minW}
      borderWidth={1}
      borderRadius="md"
      ref={containerRef}
      display="flex"
      flexDirection="column"
      position="relative"
      p={2}
    >
      <Box {...labelStyles} {...positionStyles[position]}>
        <Text fontSize={18} fontWeight="semi-bold">
          {text}
        </Text>
      </Box>
      {isCollapsable && (
        <Button
          variant="outline"
          onClick={() => {
            setCollapsedItems?.((prev) => [...prev, COMPONENT_NAME!])
          }}
          alignSelf="start"
        >
          <FiMinimize2 />
        </Button>
      )}
      <Box flex="1">{children}</Box>
    </Box>
  )
}
