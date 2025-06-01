import {
  Input,
  Badge,
  HStack,
  FieldRoot,
  FieldLabel,
  FieldErrorText,
} from "@chakra-ui/react";
import { Controller, Control, FieldErrors, UseFormReturn } from "react-hook-form";
import { ConditionalParameters, EffectConditionals } from "@/client";
import { NotificationFormValues } from "../Builder";
import { HiMinus, HiPlus } from "react-icons/hi";


function ConditionChoice({name, onClick, selected}: {name: string, onClick: () => void, selected: boolean}){
    return (
        <Badge onClick={onClick} cursor="pointer" variant={selected ? "solid" : "outline"}>
            {selected && <HiMinus />}
            {!selected && <HiPlus />}
            {name}
        </Badge>
    
    )
}


export function Conditions({
    control,
    errors,
    form,
    supported_conditional_parameters,
}: {
    control: Control<NotificationFormValues, any, NotificationFormValues>;
    errors: FieldErrors<NotificationFormValues>;
    form: UseFormReturn<NotificationFormValues, any, NotificationFormValues>;
    supported_conditional_parameters: Array<string>;
}) {


    const conditionTypes: Record<keyof ConditionalParameters, EffectConditionals> = {
        amount: "amount_over",
        count: "count_of_transactions",
    };

    const toggleCondition = (condition: EffectConditionals) => {
        if (form.getValues("condition") === condition) {
            form.setValue("condition", null);
        } else {
            form.setValue("condition", condition);
        }
    };

    return (
        <>
        <FieldRoot invalid={!!errors.condition} required mt={4}>
        <HStack>
            {supported_conditional_parameters.map((param) => (
                <ConditionChoice
                    key={param}
                    name={param}
                    onClick={() => {
                        toggleCondition(conditionTypes[param as keyof ConditionalParameters]);
                    }}
                    selected={form.getValues("condition") === conditionTypes[param as keyof ConditionalParameters]}
                />
            ))}
        </HStack>
        {errors.condition && (
          <FieldErrorText>{errors.condition.message}</FieldErrorText>
        )}
      </FieldRoot>

      {form.getValues("condition") && (
      <FieldRoot invalid={!!errors.conditional_parameters} required>
        <FieldLabel htmlFor="conditional_parameters">
          {form.getValues("condition") === "amount_over"
            ? "Amount Over"
            : "Number of Transactions"}
        </FieldLabel>
        <Controller
          control={control}
          name="conditional_parameters"
          render={({ field }) => {
            const { onChange, value } = field;
            const condition = form.getValues("condition");

            return (
              <Input
                id="conditional_parameters"
                type="number"
                min={0}
                step={condition === "amount" ? 0.01 : 1}
                value={
                  condition === "amount"
                    ? value?.amount || 0
                    : value?.count || 0
                }
                onChange={(e) => {
                  const numValue = parseFloat(e.target.value);
                  onChange(
                    condition === "amount"
                      ? { amount: numValue }
                      : { count: numValue },
                  );
                }}
                placeholder={
                  condition === "amount"
                    ? "Enter amount threshold"
                    : "Enter number of transactions"
                }
              />
            );
          }}
          rules={{
            required: "This field is required",
            validate: (value) => {
              const condition = form.getValues("condition");
              const numValue =
                condition === "amount"
                  ? value?.amount
                  : value?.count;
              if (typeof numValue !== "number" || numValue < 0) {
                return "Must be a positive number";
              }
              if (
                condition === "count_of_transactions" &&
                !Number.isInteger(numValue)
              ) {
                return "Must be a whole number";
              }
              return true;
            },
          }}
        />
        {errors.conditional_parameters && (
          <FieldErrorText>
            {errors.conditional_parameters?.message as unknown as string}
          </FieldErrorText>
        )}
      </FieldRoot>
      )}
    </>
    )
}