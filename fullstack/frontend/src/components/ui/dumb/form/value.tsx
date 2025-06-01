import {
  Input,
  FieldRoot,
  FieldLabel,
  FieldErrorText,
  Textarea,
} from "@chakra-ui/react";



export function DumbNumberField({
  name,
  label,
  register,
  errors,
}: {
  name: string;
  label: string;
  register: any;
  errors: any;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>
        {label}
      </FieldLabel>
      <Input
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
        type="number"
      />
      {errors[name] && (
        <FieldErrorText>{errors[name].message}</FieldErrorText>
      )}
    </FieldRoot>
  );
}

export function DumbTextField({
  name,
  label,
  register,
  errors,
}: {
  name: string;
  label: string;
  register: any;
  errors: any;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>
        {label}
      </FieldLabel>
      <Input
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
        type="text"
      />
      {errors[name] && (
        <FieldErrorText>{errors[name].message}</FieldErrorText>
      )}
    </FieldRoot>
  );
}

export function DumbTextareaField({
  name,
  label,
  register,
  errors,
}: {
  name: string;
  label: string;
  register: any;
  errors: any;
}) {
  return (
    <FieldRoot invalid={!!errors[name]} required>
      <FieldLabel htmlFor={name}>
        {label}
      </FieldLabel>
      <Textarea
        id={name}
        {...register(name, {
          required: `${name} is required`,
        })}
        placeholder={name}
      />
      {errors[name] && (
        <FieldErrorText>{errors[name].message}</FieldErrorText>
      )}
    </FieldRoot>
  );
}
