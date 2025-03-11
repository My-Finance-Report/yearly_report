import { ChakraProvider } from "@chakra-ui/react"
import { QueryClient, QueryClientProvider } from "@tanstack/react-query"
import { RouterProvider, createRouter } from "@tanstack/react-router"
import React from "react"
import ReactDOM from "react-dom/client"
import { ColorModeProvider } from "./components/ui/color-mode"
import { Toaster } from "./components/ui/toaster"
import { ChartColorProvider } from "./hooks/useColor"
import { routeTree } from "./routeTree.gen"

import { StrictMode } from "react"
import { OpenAPI } from "./client"
import "./index.css"
import theme from "./theme"

OpenAPI.BASE = import.meta.env.VITE_API_URL

OpenAPI.TOKEN = async () => {
  return localStorage.getItem("access_token") || ""
}

const queryClient = new QueryClient()

const router = createRouter({ routeTree })
declare module "@tanstack/react-router" {
  interface Register {
    router: typeof router
  }
}

const root = document.getElementById("root")

if (!root) {
  throw "im not sure why this would happen"
}

ReactDOM.createRoot(root).render(
  <StrictMode>
    <ChakraProvider value={theme}>
      <ColorModeProvider>
        <ChartColorProvider>
          <Toaster/>
            <QueryClientProvider client={queryClient}>
              <RouterProvider router={router} />
            </QueryClientProvider>
        </ChartColorProvider>
      </ColorModeProvider>
    </ChakraProvider>
  </StrictMode>,
)
