import { Box, Text, Input, Button } from "@chakra-ui/react";
import { Parameter, ParameterType, SelectOption } from "@/client";
import {
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
} from "@chakra-ui/react";
import { useState } from "react";

export interface ParameterProps {
  parameter: Parameter;
  onChange: (value: string | number) => void;
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
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="number" value={parameter.value || ""} onChange={wrappedChange} />
    </Box>
  );
}

function FloatParameter({ parameter, onChange }: ParameterProps) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="number" value={parameter.value || ""} onChange={wrappedChange} />
    </Box>
  );
}

function StrParameter({ parameter, onChange }: ParameterProps) {
  const [val, setVal] = useState("")
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    setVal(e.target.value)
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <Input type="text" value={val} onChange={wrappedChange} />
    <Button onClick={() => onChange(val)}>Submit</Button>
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

  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <SelectRoot
        placeholder={parameter.name}
        collection={createListCollection(formattedOptions)}
        onValueChange={(val) => {
          onChange(parameter.options?.find((option) => option.key === val.value[0]));
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
