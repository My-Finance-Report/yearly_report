import { Box } from "@chakra-ui/react"
import { useTheme } from "next-themes"
import React from "react"
import { Sankey, Tooltip } from "recharts"
import { Layer } from "recharts"

interface CustomLinkProps {
  sourceX: number
  targetX: number
  sourceY: number
  targetY: number
  sourceControlX: number
  targetControlX: number
  linkWidth: number
  index: number
  linkColor: string
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
  const [fill, setFill] = React.useState(linkColor)

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
  )
}

export interface SankeyChartProps {
  data: {
    nodes: { name: string }[]
    links: { source: number; target: number; value: number }[]
  }
  width?: number
  height?: number
}

export function GenericSankeyChart({
  data,
  width = 960,
  height = 600,
}: SankeyChartProps) {
  const testData = {
    nodes: [
      { name: "Deposits", id: 0 },
      { name: "Withdrawals", id: 1 },
      { name: "Salary", id: 2 },
      { name: "Investments", id: 3 },
      { name: "Bills", id: 4 },
      { name: "Groceries", id: 5 },
    ],
    links: [
      { source: 2, target: 0, value: 5000 },
      { source: 3, target: 0, value: 2000 },
      { source: 0, target: 4, value: 1000 },
      { source: 0, target: 5, value: 1500 },
      { source: 1, target: 4, value: 2000 },
      { source: 1, target: 5, value: 500 },
    ],
  }

  const lightModePalette = [
    "#3182CE",
    "#38A169",
    "#E53E3E",
    "#DD6B20",
    "#805AD5",
  ]
  const darkModePalette = [
    "#63B3ED",
    "#68D391",
    "#FC8181",
    "#F6AD55",
    "#B794F4",
  ]

  const theme = useTheme()
  const colorPalette =
    theme.theme === "dark" ? darkModePalette : lightModePalette

  return (
    <Box borderWidth={1} borderRadius="md" p={4}>
      <Sankey
        width={width}
        height={height}
        data={testData}
        nodeWidth={10}
        nodePadding={20}
        linkCurvature={0.5}
        iterations={32}
        link={(props) => (
          <CustomLink
            {...props}
            linkColor={colorPalette[props.index % colorPalette.length]}
          />
        )}
      >
        <Tooltip />
      </Sankey>
    </Box>
  )
}
