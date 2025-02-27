import React, { useState } from "react";
import { Box, Button } from "@chakra-ui/react";
import { Link } from "@tanstack/react-router";
import { FiChevronDown, FiChevronRight } from "react-icons/fi";

interface LabeledBoxProps {
  text: string;
  position?: "top" | "bottom" | "left" | "right"; 
  children: React.ReactNode;
  minH?: number;
  minW?: number | string;
  containerRef?: React.RefObject<HTMLDivElement>;
}

export default function LabeledBox({
  text,
  position = "top",
  children,
  minH,
  minW = "50%",
  containerRef,
}: LabeledBoxProps) {

 const [isExpanded, setIsExpanded] = useState(true);
  const labelStyles = {
    position: "absolute",
    backgroundColor: "black",
    color: "white",
    px: 2,
    fontSize: "sm",
    whiteSpace: "nowrap",
  };

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
  };

  return (
    <Box
      flex="1"
      minH={minH}
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
        {text}
      </Box>
        <Button
          variant="outline"
          onClick={() => setIsExpanded((prev) => !prev)}
          alignSelf="start"
        >
          {!isExpanded ? <FiChevronRight /> : <FiChevronDown />}
        </Button>
      {isExpanded && (
        <Box flex="1">{children}</Box>
      )}
    </Box>
  );
}
