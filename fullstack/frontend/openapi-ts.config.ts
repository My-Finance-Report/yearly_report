import { defineConfig } from "@hey-api/openapi-ts"

export default defineConfig({
  input: "./openapi.json",
  output: "./src/client",
  // exportSchemas: true,
  plugins: [
    {
      name: "@hey-api/sdk",
      // NOTE: this doesn't allow tree-shaking
      asClass: true,
      operationId: true,
      client: "legacy/axios",
      methodNameBuilder: (operation) => {
        // @ts-expect-error im not really sure
        let name: string = operation.name

        // @ts-expect-error im not really sure
        const service: string = operation.service

        if (service && name.toLowerCase().startsWith(service.toLowerCase())) {
          name = name.slice(service.length)
        }

        return name.charAt(0).toLowerCase() + name.slice(1)
      },
    },
  ],
})
