import { useTheme } from "next-themes";
import type React from "react";
import { createContext, useCallback, useContext, useRef } from "react";

interface ColorContextType {
  getColorForName: (name: string) => string;
}

const hashStringToIndex = (str: string, length: number) => {
  if (!str) return 0;
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = str.charCodeAt(i) + ((hash << 5) - hash);
  }
  return Math.abs(hash) % length;
};

const ColorContext = createContext<ColorContextType | undefined>(undefined);

export const ChartColorProvider = ({
  children,
}: {
  children: React.ReactNode;
}) => {
  const { theme } = useTheme();

  const lightModePalette = [
    "#3182CE",
    "#DD6B20",
    "#805AD5",
    "#B83280",
    "#A0AEC0",
    "#4299E1",
    "#38A169",
    "#E53E3E",
    "#C05621",
    "#D69E2E",
    "#319795",
    "#ED64A6",
    "#718096",
    "#6B46C1",
  ];

  const darkModePalette = [
    "#63B3ED",
    "#68D391",
    "#FC8181",
    "#F6AD55",
    "#B794F4",
    "#F6E05E",
    "#81E6D9",
    "#F687B3",
    "#D53F8C",
    "#4299E1",
    "#38A169",
    "#E53E3E",
    "#C05621",
    "#D69E2E",
    "#319795",
    "#ED64A6",
    "#718096",
    "#6B46C1",
  ];

  const colorPalette = theme === "dark" ? darkModePalette : lightModePalette;

  // Use a ref to store assigned colors so updating it doesn't trigger a re-render
  const assignedColorsRef = useRef<Record<string, string>>({});

  const getColorForName = useCallback(
    (name: string) => {
      if (!assignedColorsRef.current[name]) {
        assignedColorsRef.current[name] =
          colorPalette[hashStringToIndex(name, colorPalette.length)];
      }
      return assignedColorsRef.current[name];
    },
    [colorPalette],
  );

  return (
    <ColorContext.Provider value={{ getColorForName }}>
      {children}
    </ColorContext.Provider>
  );
};

export const useColorPalette = () => {
  const context = useContext(ColorContext);
  if (!context) {
    throw new Error("useColorPalette must be used within a ChartColorProvider");
  }
  return context;
};
