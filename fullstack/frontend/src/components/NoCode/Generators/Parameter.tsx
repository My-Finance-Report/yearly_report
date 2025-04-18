import { Box, Text, Input, Button, SelectItemIndicator } from "@chakra-ui/react";
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
  multi_select: MultiSelectParameter,
};

function IntParameter({ parameter, onChange }: ParameterProps) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.label || parameter.name}</Text>
      <Input type="number" value={parameter.value || parameter.default_value} onChange={wrappedChange} />
    </Box>
  );
}

function FloatParameter({ parameter, onChange }: ParameterProps) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
    <Box>

      <Text key={`${parameter.name}`}>{parameter.label || parameter.name}</Text>
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
      <Text key={`${parameter.name}`}>{parameter.label || parameter.name}</Text>
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
      <Text key={`${parameter.name}`}>{parameter.label || parameter.name}</Text>
      <SelectRoot
        collection={createListCollection(formattedOptions)}
        onValueChange={(val) => {
          onChange(parameter.options?.find((option: SelectOption) => option.key === val.value[0]));
        }}
        defaultValue={[parameter.default_value?.key]}
      >
        <SelectTrigger>
          <SelectValueText placeholder={parameter.name} />
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

function MultiSelectParameter({ parameter, onChange }: ParameterProps) {

  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  return (
    <Box>
      <Text key={`${parameter.name}`}>{parameter.name}</Text>
      <SelectRoot
        multiple
        placeholder={parameter.name}
        defaultValue={[parameter.default_value.map((v)=>v.key)]}
        collection={createListCollection(formattedOptions)}
        onValueChange={(val) => {
          onChange(val.value.map((v) => parameter.options?.find((option: SelectOption) => option.key === v)));
        }}
      >
        <SelectTrigger>
          <SelectValueText placeholder="Select a kind" />
        </SelectTrigger>
        <SelectContent>
          {formattedOptions.items.map((kind) => (
            <SelectItem key={kind.value} item={kind}>
              {kind.label}
            <SelectItemIndicator />
            </SelectItem>
          ))}
        </SelectContent>
      </SelectRoot>
    </Box>
  );
}



export function NoCodeParameter({ parameter, onChange }: ParameterProps) {
  const TheInput = MAP_TO_PARAMETER[parameter.type as ParameterType];
  return (
    <Box mt={2}>
      <TheInput parameter={parameter} onChange={onChange} />
    </Box>
  );
}
