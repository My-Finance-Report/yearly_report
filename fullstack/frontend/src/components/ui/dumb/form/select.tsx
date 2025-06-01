
import { FieldErrorText, FieldLabel, FieldRoot } from "@chakra-ui/react";
import { Controller } from "react-hook-form";
import { DumbSelect } from "@/components/ui/dumb-select";
import React from "react";

export function DumbFormSelect<T>({
    control,
    errors,
    name,
    label,
    options,
    labelExtractor,
    keyExtractor,
    placeholder,
}: {
    control: any;
    errors: any;
    name: string;
    label: string;
    options: T[];
    labelExtractor: (value: T) => string;
    keyExtractor: (value: T) => string;
    placeholder?: string;
}){
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
                  )
                }}
              />
              {errors[name] && (
                <FieldErrorText>{errors[name].message}</FieldErrorText>
              )}
            </FieldRoot>
    );
}