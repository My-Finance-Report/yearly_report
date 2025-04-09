import { useMutation, useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"
import { NoCodeShow } from "@/components/NoCode/Outputs/Show"
import {  NoCodeParameter } from "@/components/NoCode/Generators/Parameter"
import { NoCodeService,  NoCodeTool_Input,   PipelineEnd } from "@/client"
import { Container, Button,Box, Heading, Text, Badge, HoverCardRoot, HoverCardTrigger, HoverCardContent, HoverCardPositioner, Flex } from "@chakra-ui/react"
import React, { useState } from "react"
import { AddIcon, DeleteIcon } from "@chakra-ui/icons"

export const Route = createFileRoute("/_layout/_logged_in/no-code")({
  component: NoCodeBuilder,
})

function NoCodeBuilder() {
  const { data: fetchedTools, isLoading } = useQuery({
    queryKey: ["no-code-tools"],
    queryFn: () => NoCodeService.getNoCodeTool(),
  });

  const [result, setResult] = useState<PipelineEnd | null>(null)
  const [error, setError] = useState<string | null>(null)

 const mutation = useMutation({
    mutationFn: (data: NoCodeTool_Input[]) =>
      NoCodeService.saveNoCodeTool({ requestBody: data }),
    onSuccess: (data) => {
      setError(null)
      setResult(data)
    },
    onError: (error: {body: {detail: string}}) => {
      setResult(null)
      setError(error.body?.detail)
    },
  })

  function handleSavePipeline() {
    mutation.mutate(pipeline)
  }


  const [pipeline, setPipeline] = useState<NoCodeTool_Input[]>([]);

  if (isLoading) {
    return <div>Loading...</div>
  }

  if (!fetchedTools) {
    return <div>No tools found</div>
  }

  return (
    <Container maxW="lg" my={8}>
      <Heading mb={4}>No-Code Pipeline Builder</Heading>

      <Flex direction="row" gap={2} mb={6}>
        {fetchedTools.map((node) => (
          <AddNodeButton key={node.tool} setPipeline={setPipeline} tool={node} />
        ))}
      </Flex>

      <Box>
        <Text fontWeight="bold">Current Pipeline</Text>
        {pipeline.length === 0 ? (
          <Text>No nodes in pipeline yet</Text>
        ) : (
          pipeline.map((node, idx) => (
            <Node key={`${node.tool}-${idx}`} node={node} idx={idx} setPipeline={setPipeline} />
          ))
        )}
      </Box>

      <Button
        colorScheme="blue"
        mt={4}
        onClick={handleSavePipeline}
        disabled={pipeline.length === 0}
      >
        Save Pipeline
      </Button>
      <Button
        colorScheme="blue"
        mt={4}
        onClick={() => setPipeline([])}
      >
        Reset
      </Button>

      {error && (
        <Box mt={4}>
          <Text fontWeight="bold" color="red">{error}</Text>
        </Box>
      )}
      {result && (
        <Box mt={4}>
          <Text fontWeight="bold">Result</Text>
          <NoCodeShow pipelineEnd={result} />
        </Box>
      )}
    </Container>
  )
}

function AddNodeButton({ setPipeline, tool }: { setPipeline: React.Dispatch<React.SetStateAction<NoCodeTool_Input[]>>, tool: NoCodeTool_Input }) {
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
            <Button
              size="xs"
              onClick={() => setPipeline(prev => [...prev, tool])}
            >
              <AddIcon/>
            </Button>
          </Badge>

  )
}


function Node({ node, idx, setPipeline }: { node: NoCodeTool_Input, idx: number, setPipeline: React.Dispatch<React.SetStateAction<NoCodeTool_Input[]>>}) {

    const removeNode = () => {
      setPipeline(prev => prev.filter((_, i) => i !== idx))
    }

    const onChange = node.parameters && node.parameters.length > 0 ?  (e: React.ChangeEvent<HTMLInputElement>) => {
          const updatedParams = [...(node.parameters || [])];
          updatedParams[idx] = {
            ...node.parameters[idx],
            value: e.target.value,
          };
          setPipeline((prev) =>
            prev.map((n, i) =>
              i === idx ? { ...n, parameters: updatedParams } : n
            )
          );
        } : () => {}

    return (
            <Box key={`${node.tool}-${idx}`} borderWidth="1px" p={3} mb={2}>
              <Button
                size="xs"
                position="absolute"
                right={10}
                onClick={removeNode}
              >
                <DeleteIcon/>
              </Button>
              <Text fontWeight="semibold">
                Step {idx + 1}: {node.name}
              </Text>
              <Text>{node.description}</Text>
              {node.parameters && (
                node.parameters.map((param, paramIdx) => (

                  <NoCodeParameter key={`${param.name}-${paramIdx}`} parameter={param} onChange={onChange} />
                ))
              )}
            </Box>

  )
}

