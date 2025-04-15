import { Box, Text, Input } from "@chakra-ui/react";
import { Parameter, ParameterType, SelectOption } from "@/client";
import {
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
} from "@chakra-ui/react";

export interface ParameterProps {
  parameter: Parameter;
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

const MAP_TO_PARAMETER: Record<
  ParameterType,
  (props: ParameterProps) => JSX.Element
> = {
  int: IntParameter,
  string: StrParameter,
  float: FloatParameter,
  select: SelectParameter,
};

function IntParameter({ parameter, onChange }: ParameterProps) {
  const wrappedChange = (e)=>
  {onChange(e.target.value)}
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="number" value={parameter.value || ""} onChange={wrappedChange} />
    </Box>
  );
}

function FloatParameter({ parameter, onChange }: ParameterProps) {
  const wrappedChange = (e)=>
  {onChange(e.target.value)}
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="number" value={parameter.value || ""} onChange={wrappedChange} />
    </Box>
  );
}

function StrParameter({ parameter, onChange }: ParameterProps) {

  const wrappedChange = (e)=>
  {onChange(e.target.value)}

  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="text" value={parameter.value || ""} onChange={wrappedChange} />
    </Box>
  );
}

function rawSelectOptionsToSelectItems(options: SelectOption[]) {
  const blahs = options.map((options) => ({
    label: options.value,
    value: options.key,
  }));
  return { items: blahs };
}

function SelectParameter({ parameter, onChange }: ParameterProps) {
  console.log(parameter.options);

  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  return (
    <Box>
      <SelectRoot
        placeholder={parameter.name}
        collection={createListCollection(formattedOptions)}
        onValueChange={(val) => {
          console.log(val)
          onChange(val.value[0]);
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
  );
}

export function NoCodeParameter({ parameter, onChange }: ParameterProps) {
  const TheInput = MAP_TO_PARAMETER[parameter.type];
  return (
    <Box mt={2}>
      <TheInput parameter={parameter} onChange={onChange} />
    </Box>
  );
}
