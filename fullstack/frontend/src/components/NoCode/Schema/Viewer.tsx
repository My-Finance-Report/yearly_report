import { Box, Text, List, ListItem, Badge } from "@chakra-ui/react";
import { useState } from "react";

interface SchemaViewerProps {
  schema: JSONSchema;
  prevWasArray?: boolean;
  rootSchema?: JSONSchema;
  depth?: number;
  maxDepth?: number;
}

export function SchemaViewer({
  schema,
  rootSchema,
  depth = 0,
  maxDepth,
}: SchemaViewerProps) {
  if (maxDepth && depth >= maxDepth) {
    return;
  }
  const indent = depth * 3;
  const actualRoot = rootSchema || schema;
  const resolved = resolveSchema(actualRoot, schema);

  return (
    <Box pl={`${indent}px`}>
      {resolved.type && !["object", "array"].includes(resolved.type) && (
        <Text fontSize="sm" mb={2}>
          <Badge>{resolved.type}</Badge>
        </Text>
      )}

      {resolved.type === "object" && resolved.properties && (
        <>
          <List.Root variant="plain" ml={0} mt={2}>
            {Object.entries(resolved.properties).map(
              ([propName, propSchema]) => {
                return (
                  <ListItem key={propName} mb={2}>
                    <Text mr={2}>{propName}</Text>
                    <SchemaViewer
                      schema={propSchema}
                      rootSchema={actualRoot}
                      depth={depth + 1}
                      maxDepth={maxDepth}
                    />
                  </ListItem>
                );
              },
            )}
          </List.Root>
        </>
      )}

      {resolved.type === "array" && resolved.items && (
        <Box mt={2}>
          <SchemaViewer
            prevWasArray={true}
            schema={resolved.items}
            rootSchema={actualRoot}
            depth={depth + 1}
            maxDepth={maxDepth}
          />
        </Box>
      )}

      {Array.isArray(resolved.anyOf) && (
        <Box>
          {resolved.anyOf.map((sub, idx) => (
            <SchemaViewer
              key={idx}
              schema={sub}
              rootSchema={actualRoot}
              depth={depth + 1}
              maxDepth={maxDepth}
            />
          ))}
        </Box>
      )}
    </Box>
  );
}

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
  //eslint-disable-next-line
  [key: string]: any;
};

/**
 * Resolve a schema node that may have a "$ref", looking it up in
 * rootSchema.$defs if needed.
 */
export function resolveSchema(
  rootSchema: JSONSchema,
  node: JSONSchema,
): JSONSchema {
  if (!node.$ref) {
    return node;
  }

  // Example: $ref = "#/$defs/NoCodeTransaction"
  const refPath = node.$ref.replace(/^#\//, "");
  // => "$defs/NoCodeTransaction"

  const segments = refPath.split("/");
  //eslint-disable-next-line
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

export function CollapsibleSchemaRoot({ schema }: { schema: JSONSchema }) {
  const [expanded, setExpanded] = useState(false);

  // If the top-level references something in $defs, resolve it:
  const resolved = resolveSchema(schema, schema);

  const secondLayer = resolveSchema(
    schema,
    resolved.items ? resolved.items : resolved,
  );
  const format = resolved.items ? `${secondLayer.title}[]` : secondLayer.title;

  const handleToggle = () => setExpanded((prev) => !prev);

  if (resolved.type == "null") {
    return (
      <Box borderWidth={1} borderRadius="md">
        <Box
          display="flex"
          alignItems="center"
          justifyContent={"space-between"}
          cursor="pointer"
          onClick={handleToggle}
          p={3}
          mb={2}
        >
          <Badge>N/A</Badge>
        </Box>
      </Box>
    );
  }

  return (
    <Box borderWidth={1} borderRadius="md">
      <Box
        display="flex"
        alignItems="center"
        justifyContent={"space-between"}
        cursor="pointer"
        onClick={handleToggle}
        p={3}
        mb={2}
      >
        <Badge>{format}</Badge>
        <Text fontSize="xs" opacity={0.7}>
          {expanded ? "▲ collapse" : "▼ expand"}
        </Text>
      </Box>

      {expanded && (
        <Box ml={4}>
          <SchemaViewer schema={resolved} rootSchema={resolved} />
        </Box>
      )}
    </Box>
  );
}
