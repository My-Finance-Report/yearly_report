import {
  Input,
  FieldRoot,
  FieldLabel,
  FieldErrorText,
  Textarea,
  VStack,
  Box,
  Text,
  Badge,
  HStack,
} from "@chakra-ui/react";
import { useRef } from "react";

import {
  UseFormRegister,
  FieldErrors,
  FieldValues,
  Path,
} from "react-hook-form";

export function DumbNumberField<T extends FieldValues>({
  name,
  label,
  register,
  errors,
  defaultValue,
  disabled,
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
  defaultValue?: number;
  disabled?: boolean;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <Input
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
        type="number"
        defaultValue={defaultValue}
        disabled={disabled}
      />
      {errors[name] && (
        <FieldErrorText>
          {errors[name]?.message?.toString() || ""}
        </FieldErrorText>
      )}
    </FieldRoot>
  );
}

export function DumbTextField<T extends FieldValues>({
  name,
  label,
  register,
  errors,
  disabled,
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
  disabled?: boolean;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <Input
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
        disabled={disabled}
        type="text"
      />
      {errors[name] && (
        <FieldErrorText>
          {errors[name]?.message?.toString() || ""}
        </FieldErrorText>
      )}
    </FieldRoot>
  );
}

export function DumbTextareaField<T extends FieldValues>({
  name,
  label,
  register,
  errors,
  disabled,
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
  disabled?: boolean;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <Textarea
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
        disabled={disabled}
      />
      {errors[name] && (
        <FieldErrorText>
          {errors[name]?.message?.toString() || ""}
        </FieldErrorText>
      )}
    </FieldRoot>
  );
}

export function TemplateEditor<T extends FieldValues>({
  name,
  availableVariables,
  label,
  register,
  errors,
  setValue,
  disabled,
}: {
  name: Path<T>;
  availableVariables: string[];
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
  setValue: (name: Path<T>, value: string) => void;
  disabled?: boolean;
}) {
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  const insertVariable = (
    variable: string,
    textareaElement: HTMLTextAreaElement,
  ) => {
    const start = textareaElement.selectionStart;
    const end = textareaElement.selectionEnd;
    const value = textareaElement.value;

    const newValue =
      value.substring(0, start) + `{{${variable}}}` + value.substring(end);
    const newCursorPos = start + variable.length + 4;
    console.log(newValue);

    setValue(name, newValue);

    setTimeout(() => {
      textareaElement.focus();
      textareaElement.setSelectionRange(newCursorPos, newCursorPos);
    }, 0);
  };

  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <VStack w="full" align="stretch" gap={2}>
        <Textarea
          id={name}
          {...register(name, {
            required: `${name} is required`,
          })}
          ref={(el) => {
            textareaRef.current = el;
            const { ref } = register(name);
            if (typeof ref === "function") {
              ref(el);
            }
          }}
          onKeyDown={(e) => {
            const textarea = e.currentTarget;
            if (e.key === "Tab" && textarea) {
              e.preventDefault();
              insertVariable(Object.keys(availableVariables)[0], textarea);
            }
          }}
          placeholder={name}
          rows={6}
          disabled={disabled}
        />
        {availableVariables.length > 0 && (
          <Box>
            <Text fontSize="sm" mb={2}>
              Available Variables:
            </Text>
            <HStack gap={2} flexWrap="wrap">
              {availableVariables.map((variable) => (
                <Badge
                  key={variable}
                  cursor="pointer"
                  onClick={() => {
                    if (textareaRef.current) {
                      insertVariable(variable, textareaRef.current);
                    }
                  }}
                  _hover={{ bg: "blue.100" }}
                  title={variable}
                >
                  {variable}
                </Badge>
              ))}
            </HStack>
          </Box>
        )}
        {errors[name] && (
          <FieldErrorText>
            {errors[name]?.message?.toString() || ""}
          </FieldErrorText>
        )}
      </VStack>
    </FieldRoot>
  );
}
