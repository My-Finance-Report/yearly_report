import {
  Box,
  Text,
  Field,
  Input,
  Button,
  Select,
  ConditionalValue,
  Pagination,
  ButtonGroup,
  IconButton,
} from "@chakra-ui/react";
import { DisplayInfo, ParameterType, SelectOption } from "@/client";
import { Portal, createListCollection } from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { JSX } from "react";
import { FaChevronLeft, FaChevronRight } from "react-icons/fa";

export interface ParameterBase {
  name: string;
  label?: string;
  description?: string;
  type: ParameterType;
  value?: unknown;
  default_value?: unknown;
  options?: SelectOption[];
  display_info: DisplayInfo | null;
}

export interface IntParameter extends ParameterBase {
  type: "int";
  value?: number;
  default_value?: { value: number };
}

export interface FloatParameter extends ParameterBase {
  type: "float";
  value?: number;
  default_value?: { value: number };
}
export interface StringParameter extends ParameterBase {
  type: "string";
  value?: string;
  default_value?: { value: string };
}
export interface DatetimeParameter extends ParameterBase {
  type: "datetime";
  value?: string | undefined;
  default_value?: { value: string };
}
export interface SubmitParameter extends ParameterBase {
  type: "submit";
  value?: string;
  default_value?: { value: string };
}

export interface SelectParameter extends ParameterBase {
  type: "select";
  value?: SelectOption;
  default_value?: { value: SelectOption };
  options: SelectOption[];
}

export interface PaginationParameter extends ParameterBase {
  type: "pagination";
  value?: SelectOption;
  default_value?: { value: SelectOption };
  options: SelectOption[];
}

export interface MultiSelectParameter extends ParameterBase {
  type: "multi_select";
  value?: SelectOption[];
  default_value?: { value: SelectOption[] };
  options: SelectOption[];
}

export type Parameter_Output =
  | IntParameter
  | FloatParameter
  | StringParameter
  | SubmitParameter
  | DatetimeParameter
  | SelectParameter
  | PaginationParameter
  | MultiSelectParameter;

export type ParameterValueType<T extends ParameterType> = T extends
  | "int"
  | "float"
  ? number
  : T extends "string"
    ? string
    : T extends "datetime"
      ? string
      : T extends "submit"
        ? boolean
        : T extends "select"
          ? SelectOption
          : T extends "multi_select"
            ? SelectOption[]
            : T extends "pagination"
              ? SelectOption
              : never;

export type ParameterProps<T extends ParameterType> = {
  parameter: Extract<Parameter_Output, { type: T }>;
  onChange: (value: ParameterValueType<T>) => void;
};

const MAP_TO_PARAMETER = {
  int: IntParameter,
  float: FloatParameter,
  pagination: PaginationParameter,
  datetime: DatetimeParameter,
  submit: SubmitParameter,
  string: StrParameter,
  select: SelectParameter,
  multi_select: MultiSelectParameter,
} as const;

type ParameterComponent<T extends ParameterType> = (
  props: ParameterProps<T>,
) => JSX.Element | null;

const MAP_TO_PARAMETER_TYPED: {
  [K in ParameterType]: ParameterComponent<K>;
} = MAP_TO_PARAMETER;

function PaginationParameter({
  parameter,
  onChange,
}: ParameterProps<"pagination">) {
  const passedPage =
    parameter.value?.value || parameter.default_value?.value.value || "1";

  const maxPage =
    parameter.options
      ?.map((option) => Number(option.value))
      .reduce((a, b) => Math.max(a, b)) || 0;

  const [page, setPage] = useState(Number(passedPage));

  const handleSet = (e: { page: number }) => {
    setPage(e.page);
    onChange({
      key: e.page.toString(),
      value: e.page.toString(),
    });
  };

  if (maxPage === 0) {
    return null;
  }

  return (
    <Pagination.Root
      count={maxPage}
      pageSize={1}
      page={page}
      onPageChange={handleSet}
    >
      <ButtonGroup variant="ghost" size="sm" spaceX={4}>
        <Pagination.PrevTrigger asChild>
          <IconButton>
            <FaChevronLeft />
          </IconButton>
        </Pagination.PrevTrigger>

        <Pagination.Items
          render={(page) => (
            <IconButton variant={{ base: "ghost", _selected: "outline" }}>
              {page.value}
            </IconButton>
          )}
        />

        <Pagination.NextTrigger asChild>
          <IconButton>
            <FaChevronRight />
          </IconButton>
        </Pagination.NextTrigger>
      </ButtonGroup>
    </Pagination.Root>
  );
}

function IntParameter({ parameter, onChange }: ParameterProps<"int">) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    onChange(e.target.valueAsNumber);
  return (
    <Input
      maxW="100px"
      variant="subtle"
      size="sm"
      type="number"
      value={parameter.value || parameter.default_value?.value || ""}
      onChange={wrappedChange}
    />
  );
}

function SubmitParameter({ parameter, onChange }: ParameterProps<"submit">) {
  return (
    <Button
      onClick={() => {
        onChange(true);
      }}
    >
      {parameter.name}
    </Button>
  );
}

function DatetimeParameter({
  parameter,
  onChange,
}: ParameterProps<"datetime">) {
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onChange(e.target.value);
  };
  return (
    <Input
      maxW="100px"
      variant="subtle"
      size="sm"
      type="datetime-local"
      value={parameter.value}
      onChange={wrappedChange}
    />
  );
}

function FloatParameter({ parameter, onChange }: ParameterProps<"float">) {
  const initial = parameter.value ?? parameter.default_value?.value ?? "";
  const [localValue, setLocalValue] = useState<number | string>(initial);

  useEffect(() => {
    const timeout = setTimeout(() => {
      if (typeof localValue === "number") {
        onChange(localValue);
      }
    }, 300);

    return () => clearTimeout(timeout);
  }, [localValue]);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const val = e.target.valueAsNumber;
    setLocalValue(Number.isNaN(val) ? "" : val);
  };

  return <Input type="number" value={localValue} onChange={handleChange} />;
}

function StrParameter({ parameter, onChange }: ParameterProps<"string">) {
  const [val, setVal] = useState("");
  const wrappedChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    setVal(e.target.value);

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
    return null;
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  const variant =
    parameter.display_info?.size === "large"
      ? { size: "lg", minW: "420px", fontSize: 24 }
      : { size: "sm", minW: "200px", fontSize: 16 };

  return (
    <Select.Root
      collection={createListCollection(formattedOptions)}
      variant="outline"
      size={variant.size as ConditionalValue<"sm" | "md" | "lg">}
      minW={variant.minW}
      onValueChange={(val) => {
        onChange(
          parameter.options.find(
            (option: SelectOption) => option.key === val.value[0],
          )!,
        );
      }}
      defaultValue={
        parameter.default_value ? [parameter.default_value.value.key] : []
      }
    >
      <Select.HiddenSelect />
      <Select.Control>
        <Select.Trigger>
          <Select.ValueText
            fontSize={variant.fontSize}
            placeholder={parameter.label}
          />
        </Select.Trigger>
        <Select.IndicatorGroup>
          <Select.Indicator />
        </Select.IndicatorGroup>
      </Select.Control>
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
    </Select.Root>
  );
}

function MultiSelectParameter({
  parameter,
  onChange,
}: ParameterProps<"multi_select">) {
  if (!parameter.options) {
    throw new Error("parameter options missing");
  }

  const formattedOptions = rawSelectOptionsToSelectItems(parameter.options);

  return (
    <Select.Root
      size="sm"
      width={"auto"}
      defaultValue={parameter.default_value?.value.map((v) => v.key) || []}
      collection={createListCollection(formattedOptions)}
      onValueChange={(val) => {
        onChange(
          val.value.map(
            (v) =>
              parameter.options.find(
                (option: SelectOption) => option.key === v,
              )!,
          ),
        );
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
  );
}

export function NoCodeParameter<T extends ParameterType>({
  parameter,
  onChange,
}: ParameterProps<T>) {
  const TheInput = MAP_TO_PARAMETER_TYPED[
    parameter.type
  ] as ParameterComponent<T>;
  return (
    <Field.Root>
      {parameter.label && <Field.Label>{parameter.label}</Field.Label>}
      <TheInput parameter={parameter} onChange={onChange} />
      <Field.HelperText>{parameter.description}</Field.HelperText>
    </Field.Root>
  );
}
