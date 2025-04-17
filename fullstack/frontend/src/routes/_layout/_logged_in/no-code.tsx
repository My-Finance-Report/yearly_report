import { useMutation, useQuery } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { NoCodeParameter } from "@/components/NoCode/Generators/Parameter";
import { CollapsibleSchemaRoot } from "@/components/NoCode/Schema/Viewer";

import {
  NoCodeService,
  NoCodeTool,
  NoCodeWidgetOut,
  NoCodeWidgetIn,
  WidgetType,
} from "@/client";
import {
  Container,
  Button,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
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
import React, { useEffect, useState } from "react";
import { AddIcon, DeleteIcon } from "@chakra-ui/icons";

export const Route = createFileRoute("/_layout/_logged_in/no-code")({
  component: NoCodeCanvasBuilder,
});


function NoCodeCanvasBuilder() {
  const [result, setResult] = useState<NoCodeWidgetOut[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [editWidget, setEditWidget] = useState<NoCodeWidgetIn | null>(null);
  const [widgets, setWidgets] = useState<NoCodeWidgetIn[]>([]);

  function enrichWidgetOut(widget: NoCodeWidgetOut): NoCodeWidgetIn {
    const widgetIn = widgets.find( (w)=>w.name ==widget.name)
    return {
      ...widget,
      pipeline: widgetIn?.pipeline || []
    }
  }

  function handleSaveWidgets() {
    if (!widgets) {
      return;
    }
    mutation.mutate(widgets);
  }

  useEffect(() => {
    handleSaveWidgets()
  }, [widgets])

  const { data: fetchedTools } = useQuery({
    queryKey: ["no-code-tools"],
    queryFn: () => NoCodeService.getNoCodeTool(),
  });

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

  if (!fetchedTools) {
    return <div>Loading...</div>;
  }

  return (
    <Container>
      <Button onClick={() => setEditWidget({ name: "", description: "", pipeline: [], row: 1, col: 1, height: 1, width: 1, type: "value" })}>
        Add Widget
      </Button>
      {error && <Text>{error}</Text>}
      {editWidget &&
        <NoCodeWidgetBuilder setWidgets={setWidgets} widgetIn={editWidget} tools={fetchedTools} setEditWidget={setEditWidget} />
      }
      <NoCodeCanvas widgets={result} setEditWidget={(widget)=> setEditWidget(enrichWidgetOut(widget))} />
    </Container>
  );
}



function NoCodeWidgetBuilder({ setWidgets, setEditWidget, widgetIn, tools }: { setEditWidget: React.Dispatch<React.SetStateAction<NoCodeWidgetIn | null>>; setWidgets: React.Dispatch<React.SetStateAction<NoCodeWidgetIn[]>>; widgetIn: NoCodeWidgetIn | null; tools: NoCodeTool[] }) {


  const [pipeline, setPipeline] = useState<NoCodeTool[]>([]);

  const handleSaveWidget = () => {
    if (!widget.name) {
      return;
    }
    setWidgets((prev) => [...prev, { ...widget, pipeline }]);
  }

  const handleRemoveWidget = () => {
    setWidgets((prev) => prev.filter((w) => w.name !== widget.name));
  }
  const handleDoneEditing = () => {
    setEditWidget(null);
  }


  const [widget, setWidget] = useState<NoCodeWidgetIn>(widgetIn || { name: "", description: "", pipeline: [], row: 1, col: 1, height: 1, width: 1, type: "value" });


  const widgetTypes = ["value", "list", "pie_chart"].map((theType) => ({ label: theType, value: theType }))
  const formattedOptions = { items: widgetTypes }


  return (
    <Container  my={8}>
      <Heading mb={4}>No-Code Widget Builder</Heading>
      <Button onClick={() => { handleRemoveWidget(); setEditWidget(null) }}>Remove Widget</Button>
      <Button onClick={handleDoneEditing}>Done</Button>

      <Box mb={2}>
        <Text>Widget Name</Text>
        <Input
          type="text"
          value={widget.name || ""}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, name: e.target.value }))
          }
        />
      </Box>


      <Box mb={2}>
        <Text>Description</Text>
        <Input
          type="text"
          value={widget.description || ""}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, description: e.target.value }))
          }
        />
      </Box>
      <Box mb={2}>
        <Text>Row</Text>
        <Input
          type="number"
          value={widget.row || 1}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, row: parseInt(e.target.value) }))
          }
        />
      </Box>
      <Box mb={2}>
        <Text>Col</Text>
        <Input
          type="number"
          value={widget.col || 1}
          onChange={(e) =>
            setWidget((prev) => ({ ...prev, col: parseInt(e.target.value) }))
          }
        />
      </Box>

      <Box mb={2}>

        <Text>Widget Kind</Text>
        <SelectRoot
          placeholder={widget.type}
          collection={createListCollection(formattedOptions)}
          onValueChange={(val) => {
            setWidget((prev) => ({ ...prev, type: val.value[0] as WidgetType }));
          }}
        >
          <SelectTrigger>
            <SelectValueText placeholder="Select a kind" />
          </SelectTrigger>
          <SelectContent>
            {formattedOptions.items.map((kind) => (
              <SelectItem key={kind.value} item={kind}>
                {kind.label}
              </SelectItem>
            ))}
          </SelectContent>
        </SelectRoot>
      </Box>



      <Flex direction="row" wrap="wrap" gap={2} mb={6}>
        {tools.map((node) => (
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
        onClick={() => { handleSaveWidget(); setEditWidget(null) }}
        disabled={pipeline.length === 0}
      >
        Add Widget
      </Button>
      <Button colorScheme="blue" mt={4} onClick={() => setPipeline([])}>
        Reset
      </Button>
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

  //eslint-disable-next-line
  const onChange = (value: any, index: number) => {
    const updatedParams = [...(node.parameters || [])];
    updatedParams[index] = {
      // @ts-expect-error im not sure
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
      <CollapsibleSchemaRoot schema={node.input_type} />
      <CollapsibleSchemaRoot schema={node.return_type} />
    </Box>
  );
}

