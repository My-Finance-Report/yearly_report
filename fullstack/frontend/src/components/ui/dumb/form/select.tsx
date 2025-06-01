import { FieldErrorText, FieldLabel, FieldRoot } from "@chakra-ui/react";
import {
  Controller,
  Control,
  FieldErrors,
  FieldValues,
  Path,
} from "react-hook-form";
import { DumbSelect } from "@/components/ui/dumb-select";

export function DumbFormSelect<T extends FieldValues, J>({
  control,
  errors,
  name,
  label,
  options,
  labelExtractor,
  keyExtractor,
  placeholder,
}: {
  control: Control<T>;
  errors: FieldErrors<T>;
  name: Path<T>;
  label: string;
  options: J[];
  labelExtractor: (value: J) => string;
  keyExtractor: (value: J) => string;
  placeholder?: string;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required mt={4}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <Controller
        control={control}
        name={name}
        render={({ field }) => {
          const { onChange } = field;
          return (
            <DumbSelect
              selectedOption={field.value}
              setSelectedOption={onChange}
              options={options}
              labelExtractor={labelExtractor}
              keyExtractor={keyExtractor}
              placeholder={placeholder}
            />
          );
        }}
      />
      {errors[name] && (
        <FieldErrorText>
          {errors[name]?.message?.toString() || ""}
        </FieldErrorText>
      )}
    </FieldRoot>
  );
}
