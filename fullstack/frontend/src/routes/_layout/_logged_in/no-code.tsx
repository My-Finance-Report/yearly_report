import { useMutation, useQuery } from "@tanstack/react-query"
import { createFileRoute } from "@tanstack/react-router"

import { NoCodeService,  NoCodeTool_Input,  PipelineEnd } from "@/client"
import { Container, Button,Box, Heading, Text, TableBody, TableRow, TableRoot, TableCell, TableHeader, Input } from "@chakra-ui/react"
import { useState } from "react"

export const Route = createFileRoute("/_layout/_logged_in/no-code")({
  component: NoCodeBuilder,
})


function ShowValue( {pipelineEnd}: { pipelineEnd: PipelineEnd }) {
    return <Text>{pipelineEnd.result.value.value}</Text>
}

function ShowList({ pipelineEnd }: { pipelineEnd: {result: {value: {id: number, amount: number, description: string}[]}} }) {
    console.log("in listing",pipelineEnd.result.value)
    return (
        <TableRoot>
            <TableHeader>
                <TableRow>
                    {pipelineEnd.result.value.map((data, index) => {
                    if(index ===0){
                        return Object.entries(data).map(([key, _value]) => (
                            <TableCell key={key}>{key}</TableCell>
                        ))
                    }
})}
                </TableRow>
            </TableHeader>
            <TableBody>
                {pipelineEnd.result.value.map((data) => (
                    <TableRow key={data.id}>
                        {Object.entries(data).map(([key, value]) => (
                            <TableCell key={key}>{value}</TableCell>
                        ))}
                    </TableRow>
                ))}
            </TableBody>
        </TableRoot>
)

}

const MAP_TO_SHOW = {
    "show_value": ShowValue,
    "show_list": ShowList    
}

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
        console.log("success", data)
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

  function handleAddNode(node: NoCodeTool_Input) {
    setPipeline((prev) => [...prev, node]);
  }

  console.log(result)

  const Display = MAP_TO_SHOW[result?.output_type || "show_value"] || null

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
              {node.parameters && (
                <Box mt={2}>
                  {node.parameters?.map((param, paramIdx) => (
                    <>
                    <Text key={`${param.name}-${paramIdx}`}>
                      {param.name}:
                    </Text>
                    <Input
                      type={param.type === 'int' ? "number" : "text"}
                      value={param.value || ""}
                      onChange={(e) => {
                        const updatedParams = [...(node.parameters || [])];
                        updatedParams[paramIdx] = {
                          ...param,
                          value: e.target.value,
                        };
                        setPipeline((prev) =>
                          prev.map((n, i) =>
                            i === idx ? { ...n, parameters: updatedParams } : n
                          )
                        );
                      }}
                    />
                    </>
                  ))}
                </Box>
              )}
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
          {Display && <Display pipelineEnd={result} />}
        </Box>
      )}
    </Container>
  )
}
