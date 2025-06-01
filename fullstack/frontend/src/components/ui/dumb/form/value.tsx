import {
  Input,
  FieldRoot,
  FieldLabel,
  FieldErrorText,
  Textarea,
} from "@chakra-ui/react";
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
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
  defaultValue?: number;
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
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
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
}: {
  name: Path<T>;
  label: string;
  register: UseFormRegister<T>;
  errors: FieldErrors<T>;
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
      />
      {errors[name] && (
        <FieldErrorText>
          {errors[name]?.message?.toString() || ""}
        </FieldErrorText>
      )}
    </FieldRoot>
  );
}
