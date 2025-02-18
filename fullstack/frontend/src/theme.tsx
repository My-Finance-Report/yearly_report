import { createSystem, defaultConfig, defineConfig, mergeConfigs } from "@chakra-ui/react";

const theme = defineConfig({
  theme: {
    recipes: {
      Button: {
        variants: {
          custom: {
            borderRadius: "full",
            bg: "blue.500",
            color: "white",
            textTransform: "uppercase",
            _hover: { bg: "blue.600" },
            _disabled: { bg: "gray.400", cursor: "not-allowed" },
          },
        },
      },
    },
    tokens: {
      colors: {
        ui: {
          main: "#009688",
          secondary: "#EDF2F7",
          success: "#48BB78",
          danger: "#E53E3E",
          light: "#FAFAFA",
          dark: "#1A202C",
          darkSlate: "#252D3D",
          dim: "#A0AEC0",
        },
      },
    },
  },
});

const config = mergeConfigs(defaultConfig, theme);
const system = createSystem(config);

export default system;
