import type { SankeyData } from "@/client";
import {
  PopoverArrow,
  PopoverBody,
  PopoverContent,
  PopoverRoot,
  PopoverTrigger,
} from "@/components/ui/popover";
import { useColorPalette } from "@/hooks/useColor";
import { Box, Button, Text } from "@chakra-ui/react";
import React from "react";
import { FiInfo } from "react-icons/fi";
import { Sankey, Tooltip } from "recharts";
import { Layer } from "recharts";

interface CustomLinkProps {
  sourceX: number;
  targetX: number;
  sourceY: number;
  targetY: number;
  sourceControlX: number;
  targetControlX: number;
  linkWidth: number;
  index: number;
  linkColor: string;
}

const CustomLink = ({
  sourceX,
  targetX,
  sourceY,
  targetY,
  sourceControlX,
  targetControlX,
  linkWidth,
  index,
  linkColor,
}: CustomLinkProps) => {
  const [fill, setFill] = React.useState(linkColor);

  return (
    <Layer key={`CustomLink${index}`}>
      <path
        d={`
          M${sourceX},${sourceY + linkWidth / 2}
          C${sourceControlX},${sourceY + linkWidth / 2}
            ${targetControlX},${targetY + linkWidth / 2}
            ${targetX},${targetY + linkWidth / 2}
          L${targetX},${targetY - linkWidth / 2}
          C${targetControlX},${targetY - linkWidth / 2}
            ${sourceControlX},${sourceY - linkWidth / 2}
            ${sourceX},${sourceY - linkWidth / 2}
          Z
        `}
        fill={fill}
        strokeWidth="0"
        opacity={0.8}
        onMouseEnter={() => setFill("rgba(0, 136, 254, 0.5)")}
        onMouseLeave={() => setFill(linkColor)}
      />
    </Layer>
  );
};

function isValidateData(data: SankeyData) {
  if (!data) return false;
  if (!data.nodes) return false;
  if (!data.links) return false;
  if (data.nodes.length === 0) return false;
  if (data.links.length === 0) return false;

  return true;
}

export interface SankeyChartProps {
  data: SankeyData;
  width?: number;
  height?: number;
}

export function GenericSankeyChart({
  data,
  width = 950,
  height = 600,
}: SankeyChartProps) {
  const { getColorForName } = useColorPalette();

  if (!isValidateData(data)) {
    return (
      <Text>
        No data, be sure to configure the sankey chart by clicking the "Gear"
        icon{" "}
      </Text>
    );
  }

  return (
    <Box p={4}>
      <Sankey
        width={width}
        height={height}
        data={data}
        nodeWidth={10}
        nodePadding={20}
        linkCurvature={0.5}
        iterations={32}
        link={(props) => (
          <CustomLink
            {...props}
            linkColor={() => getColorForName(props.payload.target.name)}
          />
        )}
      >
        <Tooltip />
      </Sankey>
    </Box>
  );
}

export const Desc = ({
  description,
  mt,
}: {
  description: string | undefined;
  mt?: string | undefined;
}) => {
  if (!description) {
    return null;
  }

  return (
    <PopoverRoot>
      <PopoverTrigger asChild>
        <Button size="sm" mt={mt} variant="outline">
          <FiInfo />
        </Button>
      </PopoverTrigger>
      <PopoverContent>
        <PopoverArrow />
        <PopoverBody>
          <Text my="4">{description}</Text>
        </PopoverBody>
      </PopoverContent>
    </PopoverRoot>
  );
};
