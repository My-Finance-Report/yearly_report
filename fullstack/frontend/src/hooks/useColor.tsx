import React, { createContext, useContext, useState } from "react";
import { useTheme } from "next-themes";

interface ColorContextType {
  getColorForName: (name: string) => string;
  getAssignedColors: () => Record<string, string>;
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

export const ChartColorProvider = ({ children }: { children: React.ReactNode }) => {
  const { theme } = useTheme();

  const lightModePalette = [
    "#3182CE", "#38A169", "#E53E3E", "#DD6B20", "#805AD5",
    "#D69E2E", "#319795", "#B83280", "#A0AEC0", "#ED64A6"
  ];

  const darkModePalette = [
    "#63B3ED", "#68D391", "#FC8181", "#F6AD55", "#B794F4",
    "#F6E05E", "#81E6D9", "#F687B3", "#CBD5E0", "#D53F8C"
  ];

  const colorPalette = theme === "dark" ? darkModePalette : lightModePalette;

  const [assignedColors, setAssignedColors] = useState<Record<string, string>>({});

  const getColorForName = (name: string) => {
    if (!assignedColors[name]) {
      const color = colorPalette[hashStringToIndex(name, colorPalette.length)];
      console.log(assignedColors)
      console.log('assign', name, color)
      setAssignedColors((prev) => ({
        ...prev,
        [name]: color,
      }));

    }

    return assignedColors[name] || colorPalette[hashStringToIndex(name, colorPalette.length)]; 
  };

  const getAssignedColors = () => assignedColors;

  return (
    <ColorContext.Provider value={{ getColorForName, getAssignedColors }}>
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
