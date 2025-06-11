import { Badge, HStack, FieldRoot, FieldErrorText } from "@chakra-ui/react";
import { Control, FieldErrors, UseFormReturn } from "react-hook-form";
import { EffectConditionals } from "@/client";
import { NotificationFormValues } from "../Builder";
import { HiMinus, HiPlus } from "react-icons/hi";
import { DumbNumberField } from "@/components/ui/dumb/form/value";

function ConditionChoice({
  name,
  onClick,
  selected,
  disabled,
}: {
  name: EffectConditionals;
  onClick: () => void;
  selected: boolean;
  disabled?: boolean;
}) {
  return (
    <Badge
      onClick={disabled ? undefined : onClick}
      cursor={disabled ? "not-allowed" : "pointer"}
      variant={selected ? "solid" : "outline"}
    >
      {selected && <HiMinus />}
      {!selected && <HiPlus />}
      {name}
    </Badge>
  );
}

type FormContext = unknown;

export function Conditions({
  control,
  errors,
  form,
  supported_conditional_parameters,
  disabled,
}: {
  control: Control<NotificationFormValues, FormContext, NotificationFormValues>;
  errors: FieldErrors<NotificationFormValues>;
  form: UseFormReturn<
    NotificationFormValues,
    FormContext,
    NotificationFormValues
  >;
  supported_conditional_parameters: EffectConditionals[] | undefined;
  disabled?: boolean;
}) {
  const conditionToConditionParameter: Record<
    EffectConditionals,
    React.ReactNode
  > = {
    amount_over: (
      <DumbNumberField
        key="amount"
        name="conditional_parameters.amount"
        label="Amount"
        register={control.register}
        errors={errors}
        defaultValue={10}
        disabled={disabled}
      />
    ),
    count_of_transactions: (
      <DumbNumberField
        key="count"
        name="conditional_parameters.count"
        label="Count"
        register={control.register}
        errors={errors}
        defaultValue={1}
        disabled={disabled}
      />
    ),
    unconditional: null,
  };

  const currentCondition = form.watch("condition");

  const toggleCondition = (condition: EffectConditionals) => {
    if (currentCondition === condition) {
      form.setValue("condition", "unconditional");
    } else {
      form.setValue("condition", condition);
    }
  };

  return (
    <>
      <FieldRoot invalid={!!errors.condition} required mt={4}>
        <HStack>
          {supported_conditional_parameters?.map((param) => (
            <ConditionChoice
              key={param}
              name={param}
              onClick={() => {
                toggleCondition(param);
              }}
              selected={currentCondition === param}
              disabled={disabled}
            />
          ))}
        </HStack>
        {errors.condition && (
          <FieldErrorText>{errors.condition.message}</FieldErrorText>
        )}
      </FieldRoot>
      {currentCondition !== "unconditional" &&
        conditionToConditionParameter[currentCondition]}
    </>
  );
}
