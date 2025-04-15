import { useMutation, useQuery } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { NoCodeShow } from "@/components/NoCode/Outputs/Show";
import { NoCodeParameter } from "@/components/NoCode/Generators/Parameter";
import {
  NoCodeService,
  NoCodeTool,
  NoCodeWidgetOut,
  NoCodeWidgetIn,
} from "@/client";
import {
  Container,
  Button,
  Box,
  Heading,
  Text,
  Badge,
  HoverCardRoot,
  HoverCardTrigger,
  HoverCardContent,
  HoverCardPositioner,
  Flex,
  Input,
} from "@chakra-ui/react";
import React, { useState } from "react";
import { AddIcon, DeleteIcon } from "@chakra-ui/icons";

export const Route = createFileRoute("/_layout/_logged_in/no-code")({
  component: NoCodeBuilder,
});

function NoCodeBuilder() {
  const { data: fetchedTools, isLoading } = useQuery({
    queryKey: ["no-code-tools"],
    queryFn: () => NoCodeService.getNoCodeTool(),
  });

  const [result, setResult] = useState<NoCodeWidgetOut[]>([]);
  const [error, setError] = useState<string | null>(null);

  const mutation = useMutation({
    mutationFn: (data: NoCodeWidgetIn[]) =>
      NoCodeService.saveNoCodeTool({ requestBody: data }),
    onSuccess: (data) => {
      setError(null);
      setResult(data);
    },
    onError: (error: { body: { detail: string } }) => {
      setResult([]);
      setError(error.body?.detail);
    },
  });

  const [pipeline, setPipeline] = useState<NoCodeTool[]>([]);
  const [widget, setWidget] = useState<NoCodeWidgetIn>({
    name: "",
    description: "",
    pipeline: [],
    row: 1,
    col: 1,
    height: 1,
    width: 1,
    type: "value",
  });

  function handleSaveWidget() {
    if (!widget || !pipeline) {
      return;
    }
    mutation.mutate([{ ...widget, pipeline }]);
  }

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (!fetchedTools) {
    return <div>No tools found</div>;
  }

  return (
    <Container maxW="lg" my={8}>
      <Heading mb={4}>No-Code Widget Builder</Heading>

      <Box>
        <Text>Widget Name</Text>
        <Input
          type="text"
          value={widget.name || ""}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, name: e.target.value }))
          }
        />
      </Box>


      <Box>
        <Text>Description</Text>
        <Input
          type="text"
          value={widget.description || ""}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, description: e.target.value }))
          }
        />
      </Box>
      <Box>
        <Text>row</Text>
        <Input
          type="number"
          value={widget.row || 1}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, row: e.target.value }))
          }
        />
      </Box>
      <Box>
        <Text>col</Text>
        <Input
          type="number"
          value={widget.col || 1}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, col: e.target.value }))
          }
        />
      </Box>





      <Flex direction="row" gap={2} mb={6}>
        {fetchedTools.map((node) => (
          <AddNodeButton
            key={node.tool}
            setPipeline={setPipeline}
            tool={node}
          />
        ))}
      </Flex>

      <Box>
        <Text fontWeight="bold">Current Pipeline</Text>
        {pipeline.length === 0 ? (
          <Text>No nodes in pipeline yet</Text>
        ) : (
          pipeline.map((node, idx) => (
            <Node
              key={`${node.tool}-${idx}`}
              node={node}
              idx={idx}
              setPipeline={setPipeline}
            />
          ))
        )}
      </Box>

      <Button
        colorScheme="blue"
        mt={4}
        onClick={handleSaveWidget}
        disabled={pipeline.length === 0}
      >
        Save Pipeline
      </Button>
      <Button colorScheme="blue" mt={4} onClick={() => setPipeline([])}>
        Reset
      </Button>

      {error && (
        <Box mt={4}>
          <Text fontWeight="bold" color="red">
            {error}
          </Text>
        </Box>
      )}
      {result && (
        <Box mt={4}>
          <Text fontWeight="bold">Result</Text>
          {result.map((widget) => (
            <NoCodeShow widget={widget} />
          ))}
        </Box>
      )}
    </Container>
  );
}

function AddNodeButton({
  setPipeline,
  tool,
}: {
  setPipeline: React.Dispatch<React.SetStateAction<NoCodeTool[]>>;
  tool: NoCodeTool;
}) {
  return (
    <Badge key={tool.tool} borderWidth="1px">
      <HoverCardRoot>
        <HoverCardTrigger>
          <Text fontWeight="semibold">{tool.name}</Text>
        </HoverCardTrigger>
        <HoverCardPositioner>
          <HoverCardContent>
            <Text>{tool.description}</Text>
          </HoverCardContent>
        </HoverCardPositioner>
      </HoverCardRoot>
      <Button size="xs" onClick={() => setPipeline((prev) => [...prev, tool])}>
        <AddIcon />
      </Button>
    </Badge>
  );
}

function Node({
  node,
  idx,
  setPipeline,
}: {
  node: NoCodeTool;
  idx: number;
  setPipeline: React.Dispatch<React.SetStateAction<NoCodeTool[]>>;
}) {
  const removeNode = () => {
    setPipeline((prev) => prev.filter((_, i) => i !== idx));
  };

  const onChange = (value, index) => {
    console.log(value);
    const updatedParams = [...(node.parameters || [])];
    updatedParams[index] = {
      ...node.parameters[index],
      value,
    };
    setPipeline((prev) =>
      prev.map((n, i) => (i === idx ? { ...n, parameters: updatedParams } : n))
    );
  };

  return (
    <Box key={`${node.tool}-${idx}`} borderWidth="1px" p={3} mb={2}>
      <Button size="xs" position="absolute" right={10} onClick={removeNode}>
        <DeleteIcon />
      </Button>
      <Text fontWeight="semibold">
        Step {idx + 1}: {node.name}
      </Text>
      <Text>{node.description}</Text>
      {node.parameters &&
        node.parameters.map((param, paramIdx) => (
          <NoCodeParameter
            key={`${param.name}-${paramIdx}`}
            parameter={param}
            onChange={(val) => onChange(val, paramIdx)}
          />
        ))}
    </Box>
  );
}
