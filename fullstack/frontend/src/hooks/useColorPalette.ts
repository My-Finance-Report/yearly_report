import { useTheme } from "next-themes"

export const useColorPalette = () => {
  const theme = useTheme()

  const lightModePalette = [
    "#3182CE", // Blue
    "#38A169", // Green
    "#E53E3E", // Red
    "#DD6B20", // Orange
    "#805AD5", // Purple
    "#D69E2E", // Yellow
    "#319795", // Teal
    "#B83280", // Pink
  ]

  const darkModePalette = [
    "#63B3ED", // Light Blue
    "#68D391", // Light Green
    "#FC8181", // Light Red
    "#F6AD55", // Light Orange
    "#B794F4", // Light Purple
    "#F6E05E", // Light Yellow
    "#81E6D9", // Light Teal
    "#F687B3", // Light Pink
    "#A0AEC0", // Light Gray
  ]

  return theme.theme === "dark" ? darkModePalette : lightModePalette
}
