import { Box, Heading, Text, List, ListItem, Badge } from "@chakra-ui/react";

interface SchemaViewerProps {
  schema: JSONSchema;
  rootSchema?: JSONSchema;  
  depth?: number;           
}

export function SchemaViewer({
  schema,
  rootSchema,
  depth = 0,
}: SchemaViewerProps) {
  const indent = depth * 6;
  const actualRoot = rootSchema || schema; 
  const resolved = resolveSchema(actualRoot, schema);

  return (
    <Box pl={`${indent}px`} borderLeft={depth > 0 ? "1px solid #ccc" : "none"}>

      {resolved.title && (
        <Heading as="h4" size="sm" mb={1}>
          {resolved.title}
        </Heading>
      )}


      {resolved.type &&
        !["object", "array"].includes(resolved.type) && (
          <Text fontSize="sm" mb={2}>
            <Badge>{resolved.type}</Badge>
          </Text>
        )}

      {resolved.type === "object" && resolved.properties && (
        <>
          <Text fontSize="sm">
            <Badge>Object{" "}
            {resolved.required && resolved.required.length > 0 && (
              <>
                {resolved.required.join(", ")}
              </>
            )}
            </Badge>
          </Text>
          <List.Root variant="plain" ml={0} mt={2}>
            {Object.entries(resolved.properties).map(([propName, propSchema]) => {
              return (
                <ListItem key={propName} mb={2}>
                  <Text fontWeight="bold" minWidth={150} mr={5} textAlign={"end"}>
                    {propName}
                  </Text>
                  <SchemaViewer
                    schema={propSchema}
                    rootSchema={actualRoot}
                    depth={depth + 1}
                  />
                </ListItem>
              );
            })}
          </List.Root>
        </>
      )}

      {resolved.type === "array" && resolved.items && (
        <Box mt={2}>
          <Text fontSize="sm">
            <strong>Array of:</strong>
          </Text>
          <SchemaViewer
            schema={resolved.items}
            rootSchema={actualRoot}
            depth={depth + 1}
          />
        </Box>
      )}

      {Array.isArray(resolved.anyOf) && (
        <Box mt={2}>
          <Text fontSize="sm" mb={1}>
            Union
          </Text>
          <List.Root variant="plain" ml={4}>
            {resolved.anyOf.map((sub, idx) => (
              <ListItem key={idx} mb={2}>
                <SchemaViewer schema={sub} rootSchema={actualRoot} depth={depth + 1} />
              </ListItem>
            ))}
          </List.Root>
        </Box>
      )}


    </Box>
  );
}

// schema-utils.ts
export type JSONSchema = {
  $defs?: Record<string, JSONSchema>;
  $ref?: string;
  type?: string;
  title?: string;
  description?: string;
  properties?: Record<string, JSONSchema>;
  items?: JSONSchema;
  required?: string[];
  anyOf?: JSONSchema[];
  // Add more keywords if you need them: oneOf, enum, etc.
  [key: string]: any;
};

/**
 * Resolve a schema node that may have a "$ref", looking it up in
 * rootSchema.$defs if needed.
 */
export function resolveSchema(rootSchema: JSONSchema, node: JSONSchema): JSONSchema {
  if (!node.$ref) {
    return node;
  }

  // Example: $ref = "#/$defs/NoCodeTransaction"
  const refPath = node.$ref.replace(/^#\//, ""); 
  // => "$defs/NoCodeTransaction"

  const segments = refPath.split("/");
  let current: any = rootSchema;
  for (const seg of segments) {
    if (seg in current) {
      current = current[seg];
    } else {
      console.warn(`Could not resolve path segment "${seg}" in schema`);
      return node; // fallback to the original node
    }
  }

  // Merge the resolved schema with any extra fields in 'node' 
  // (like "title", if it’s placed alongside $ref)
  return {
    ...current,
    ...node,
  };
}
