import { useMutation, useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

import { NoCodeService, NoCodeTool } from "@/client"
import { Container, Button,Box, Heading, Text } from "@chakra-ui/react"
import { useState } from "react"

export const Route = createFileRoute("/_layout/_logged_in/no-code")({
  component: NoCodeBuilder,
})


function NoCodeBuilder() {
  const { data: fetchedTools, isLoading } = useQuery({
    queryKey: ["no-code-tools"],
    queryFn: () => NoCodeService.getNoCodeTool(),
  });

 const mutation = useMutation({
    mutationFn: (data: NoCodeTool[]) =>
      NoCodeService.saveNoCodeTool({ requestBody: data }),
    onSuccess: () => {
        console.log("success")
    },
    onError: () => {
        console.log("error")
    },
  })

  function handleSavePipeline() {
    mutation.mutate(pipeline)
  }


  // We'll store the pipeline as an array of chosen nodes
  const [pipeline, setPipeline] = useState<NoCodeTool[]>([]);

  if (isLoading) {
    return <div>Loading...</div>
  }

  if (!fetchedTools) {
    return <div>No tools found</div>
  }

  function handleAddNode(node: NoCodeTool) {
    setPipeline((prev) => [...prev, node]);
  }

  return (
    <Container maxW="lg" my={8}>
      <Heading mb={4}>No-Code Pipeline Builder</Heading>

      <Box mb={6}>
        <Text fontWeight="bold">Available Nodes</Text>
        {fetchedTools.map((node) => (
          <Box key={node.tool} borderWidth="1px" p={3} mb={2}>
            <Text fontWeight="semibold">{node.name}</Text>
            <Text>{node.description}</Text>
            <Button
              size="sm"
              mt={2}
              onClick={() => handleAddNode(node)}
            >
              Add to Pipeline
            </Button>
          </Box>
        ))}
      </Box>

      <Box>
        <Text fontWeight="bold">Current Pipeline</Text>
        {pipeline.length === 0 ? (
          <Text>No nodes in pipeline yet</Text>
        ) : (
          pipeline.map((node, idx) => (
            <Box key={`${node.tool}-${idx}`} borderWidth="1px" p={3} mb={2}>
              <Text fontWeight="semibold">
                Step {idx + 1}: {node.name}
              </Text>
              <Text>{node.description}</Text>
            </Box>
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
    </Container>
  )


}
