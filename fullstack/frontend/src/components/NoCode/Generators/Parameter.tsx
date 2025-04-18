import { Box, Text, Field, Input, Button,  Select } from "@chakra-ui/react";
import { ParameterType, SelectOption } from "@/client";
import {
  Portal,
  createListCollection,
} from "@chakra-ui/react";
import { useState } from "react";


export interface ParameterBase {
  name: string;
  label?: string;
  description?: string;
  type: ParameterType;
  value?: unknown;
  default_value?: unknown;
  options?: SelectOption[];
}

export interface IntParameter extends ParameterBase {
  type: "int";
  value?: number;
  default_value?: number;
}

export interface FloatParameter extends ParameterBase {
  type: "float";
  value?: number;
  default_value?: number;
}
export interface StringParameter extends ParameterBase {
  type: "string";
  value?: string;
  default_value?: string;
}
export interface SelectParameter extends ParameterBase {
  type: "select";
  value?: SelectOption;
  default_value?: SelectOption;
  options: SelectOption[];
}

export interface MultiSelectParameter extends ParameterBase {
  type: "multi_select";
  value?: SelectOption[];
  default_value?: SelectOption[];
  options: SelectOption[];
}

export type Parameter_Output =
  | IntParameter
  | FloatParameter
  | StringParameter
  | SelectParameter
  | MultiSelectParameter;

type ParameterValueType<T extends ParameterType> =
  T extends "int" | "float" ? number :
  T extends "string" ? string :
  T extends "select" ? SelectOption :
  T extends "multi_select" ? SelectOption[] :
  never;

type ParameterProps<T extends ParameterType> = {
    parameter: Extract<Parameter_Output, { type: T }>;
    onChange: (value: ParameterValueType<T>) => void;
  };

const MAP_TO_PARAMETER = {
  int: IntParameter,
  float: FloatParameter,
  string: StrParameter,
  select: SelectParameter,
  multi_select: MultiSelectParameter,
} as const;

type ParameterComponent<T extends ParameterType> = (
  props: ParameterProps<T>
) => JSX.Element;

const MAP_TO_PARAMETER_TYPED: {
  [K in ParameterType]: ParameterComponent<K>;
} = MAP_TO_PARAMETER;



function IntParameter({ parameter, onChange }: ParameterProps<"int">) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
      <Input maxW="100px" variant='subtle' size="sm" type="number" value={parameter.value || parameter.default_value || ""} onChange={wrappedChange} />
  );
}

function FloatParameter({ parameter, onChange }: ParameterProps<"float">) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
      <Input type="number" value={parameter.value || parameter.default_value || ""} onChange={wrappedChange} />
  );
}

function StrParameter({ parameter, onChange }: ParameterProps<"string">) {
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

function SelectParameter({ parameter, onChange }: ParameterProps<"select">) {

  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

 return (
    <Select.Root
      collection={createListCollection(formattedOptions)}
      variant='subtle'
      size="sm"
      width={'auto'}
      onValueChange={(val) => {
          onChange(parameter.options.find((option: SelectOption) => option.key === val.value[0])!);
        }}
      defaultValue={parameter.default_value ? [parameter.default_value.key] : []}
    >
      <Select.HiddenSelect />
      <Select.Control>
        <Select.Trigger>
          <Select.ValueText placeholder={parameter.label || parameter.name} />
        </Select.Trigger>
        <Select.IndicatorGroup>
          <Select.Indicator />
        </Select.IndicatorGroup>
      </Select.Control>
      <Portal>
        <Select.Positioner>
          <Select.Content>
            {formattedOptions.items.map((option) => (
              <Select.Item item={option} key={option.value}>
                <Select.ItemText>{option.label}</Select.ItemText>
                <Select.ItemIndicator />
              </Select.Item>
            ))}
          </Select.Content>
        </Select.Positioner>
      </Portal>
    </Select.Root>
  )
}


function MultiSelectParameter({ parameter, onChange }: ParameterProps<"multi_select">) {

  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  return (
    <Select.Root
      size="sm"
      width={'auto'}
      defaultValue={parameter.default_value?.map((v) => v.key) || []}
      collection={createListCollection(formattedOptions)}
      onValueChange={(val) => {
          onChange(val.value.map((v) => parameter.options.find((option: SelectOption) => option.key === v)!));
        }}
    >
      <Select.HiddenSelect />
      <Select.Control>
        <Select.Trigger>
          <Select.ValueText placeholder={parameter.label || parameter.name} />
        </Select.Trigger>
        <Select.IndicatorGroup>
          <Select.Indicator />
        </Select.IndicatorGroup>
      </Select.Control>
      <Portal>
        <Select.Positioner>
          <Select.Content>
            {formattedOptions.items.map((option) => (
              <Select.Item item={option} key={option.value}>
                <Select.ItemText>{option.label}</Select.ItemText>
                <Select.ItemIndicator />
              </Select.Item>
            ))}
          </Select.Content>
        </Select.Positioner>
      </Portal>
    </Select.Root>
  )

}


export function NoCodeParameter<T extends ParameterType>({
  parameter,
  onChange,
}: ParameterProps<T>) {
  const TheInput = MAP_TO_PARAMETER_TYPED[parameter.type] as ParameterComponent<T>;
  return (
    <Field.Root>
      <Field.Label>
        {parameter.label || parameter.name}
      </Field.Label>
      <TheInput parameter={parameter} onChange={onChange} />
      <Field.HelperText>{parameter.description}</Field.HelperText>
    </Field.Root>
  );
}