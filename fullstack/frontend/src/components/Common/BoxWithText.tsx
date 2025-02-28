import { Box, Button, Text } from "@chakra-ui/react"
import type React from "react"
import { FiChevronDown, FiChevronRight } from "react-icons/fi"

interface LabeledBoxProps {
  text: string
  position?: "top" | "bottom" | "left" | "right"
  children: React.ReactNode
  minH?: number
  maxW?: number | string
  minW?: number | string
  expandable?: boolean
  containerRef?: React.RefObject<HTMLDivElement>
  width?: number | string
  isExpanded?: boolean
  setIsExpanded?: React.Dispatch<React.SetStateAction<boolean>>
}

export default function LabeledBox({
  text,
  position = "top",
  children,
  isExpanded,
  expandable = true,
  setIsExpanded,
  minH,
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

  return (
    <Box
      width={width}
      minH={minH}
      maxW={maxW}
      minW={minW}
      alignItems={'center'}
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
      <Button
        variant="outline"
        onClick={() => {
          setIsExpanded?.((prev) => !prev)
        }}
        alignSelf="start"
      >
        {!isExpanded && expandable && <FiChevronRight />}
        {isExpanded && expandable && <FiChevronDown />}
      </Button>
      {isExpanded && expandable && <Box flex="1">{children}</Box>}
    </Box>
  )
}
