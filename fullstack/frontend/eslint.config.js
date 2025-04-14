import js from "@eslint/js"
import react from "eslint-plugin-react"
import globals from "globals"
import tseslint from "typescript-eslint"

export default [
  // Base JavaScript settings
  js.configs.recommended,

  // TypeScript ESLint configs
  ...tseslint.configs.recommended,

  // React plugin with JSX settings
  {
    ...react.configs.flat.recommended,
    settings: {
      react: {
        version: "18.2",
      },
    },
    rules: {
      "react/react-in-jsx-scope": "off", // This should turn off the JSX scope rule
    },
  },

  // Custom project rules
  {
    files: ["**/*.{js,mjs,cjs,ts,jsx,tsx}"],
    languageOptions: {
      globals: globals.browser,
    },
  },

  {
    ignores: ["**/node_modules/**", "**/.vite/**", "**/src/client/**"],
  },
]
