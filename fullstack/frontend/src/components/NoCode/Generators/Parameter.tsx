import { Box, Text, Input } from "@chakra-ui/react"
import { Parameter, ParameterType } from "@/client"

export interface ParameterProps{
  parameter: Parameter
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void
}

const MAP_TO_PARAMETER: Record<ParameterType, (props: ParameterProps) => JSX.Element> = {
    'int': IntParameter,
    'string': StrParameter,
    'float': FloatParameter    
}



function IntParameter({ parameter, onChange }: ParameterProps){
    return (
        <Box>
    <Text key={`${parameter.name}`}>
        {parameter.name}
      </Text>
      <Input
        type="number"
        value={parameter.value || ""}
        onChange={onChange}
      />
</Box>
)
}

function FloatParameter({ parameter, onChange }: ParameterProps){
    return (
        <Box>
    <Text key={`${parameter.name}`}>
        {parameter.name}
      </Text>
      <Input
        type="number"
        value={parameter.value || ""}
        onChange={onChange}
      />
    </Box>
)
}




function StrParameter({ parameter, onChange }: ParameterProps){
    return (
        <Box>
    <Text key={`${parameter.name}`}>
        {parameter.name}
      </Text>
      <Input
        type="text"
        value={parameter.value || ""}
        onChange={onChange}
      />
</Box>
)
}



export function NoCodeParameter({ parameter, onChange }: ParameterProps){

  const TheInput = MAP_TO_PARAMETER[parameter.type]
  return (
    <Box mt={2}>
      <TheInput
        parameter={parameter}
        onChange={onChange}
      />
    </Box>
  )
}