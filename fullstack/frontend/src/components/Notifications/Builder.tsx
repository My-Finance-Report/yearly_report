import { NoCodeService } from "@/client";
import React, { useEffect } from "react";
import {
  Box,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
  Button,
  HStack,
  Input,
  FieldRoot,
  FieldLabel,
  Textarea,
  Card,
  VStack,
  FieldErrorText,
} from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  EffectOut,
  EventType,
  EffectType,
  EffectConditionals,
  EffectCreate,
  EffectUpdate,
} from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";
import { Controller, useForm } from "react-hook-form";

export interface NotificationFormValues {
  id?: number;
  name: string;
  template: string;
  subject: string;
  effect_type: EffectType;
  event_type: EventType;
  frequency_days: number;
  condition: EffectConditionals;
  conditional_parameters: Record<string, number>;
}

interface CreateFormProps {
  selectedEffect: EffectOut;
  setFormValues: (values: NotificationFormValues) => void;
}

export function CreateForm({ selectedEffect, setFormValues }: CreateFormProps) {
  const form = useForm<NotificationFormValues>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      name: selectedEffect.name,
      template: selectedEffect.config.template,
      subject: selectedEffect.config.subject,
      effect_type: selectedEffect.effect_type,
      event_type: selectedEffect.event_type,
      frequency_days: selectedEffect.config.frequency_days,
      condition: selectedEffect.condition,
      conditional_parameters: selectedEffect.conditional_parameters,
    },
  });

  useEffect(() => {
    if (selectedEffect) {
      form.reset({
        name: selectedEffect.name,
        template: selectedEffect.config.template,
        subject: selectedEffect.config.subject,
        effect_type: selectedEffect.effect_type,
        event_type: selectedEffect.event_type,
        frequency_days: selectedEffect.config.frequency_days,
        condition: selectedEffect.condition,
        conditional_parameters: selectedEffect.conditional_parameters,
      });
    }
  }, [selectedEffect, form]);

  useEffect(() => {
    const subscription = form.watch((data) => {
      setFormValues(data as NotificationFormValues);
    });

    return () => subscription.unsubscribe();
  }, [form, setFormValues]);

  const {
    register,
    handleSubmit,
    reset,
    control,
    formState: { errors, isSubmitting },
  } = form;
  const showToast = useCustomToast();
  const queryClient = useQueryClient();

  const eventTypes: EventType[] = ["new_transaction", "new_account_linked"];

  const conditionTypes: EffectConditionals[] = [
    "amount_over",
    "count_of_transactions",
  ];

  const effectTypes: EffectType[] = ["email", "in_app"];

  const updateMutation = useMutation({
    mutationFn: ({
      data,
      id,
    }: {
      data: NotificationFormValues;
      id: number;
    }) => {
      const body: EffectUpdate = {
        name: data.name,
        effect_type: data.effect_type,
        event_type: data.event_type,
        frequency_days: data.frequency_days,
        template: data.template,
        subject: data.subject,
        condition: data.condition,
        conditional_parameters: data.conditional_parameters,
      };

      return NoCodeService.updateEffect({ effectId: id, requestBody: body });
    },
    onSuccess: () => {
      showToast(
        "Notification updated",
        "Notification updated successfully.",
        "success",
      );
      reset();
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast("Error updating notification", error.message, "error");
    },
  });
  const createMutation = useMutation({
    mutationFn: (data: NotificationFormValues) => {
      const body: EffectCreate = {
        name: data.name,
        effect_type: data.effect_type,
        event_type: data.event_type,
        frequency_days: data.frequency_days,
        template: data.template,
        subject: data.subject,
        condition: data.condition,
        conditional_parameters:
          data.condition === "amount_over"
            ? { amount_over: data.conditional_parameters.amount_over }
            : { count: data.conditional_parameters.count },
      };

      return NoCodeService.createEffect({ requestBody: body });
    },
    onSuccess: () => {
      showToast(
        "Notification created",
        "Notification created successfully.",
        "success",
      );
      reset();
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast("Error creating notification", error.message, "error");
    },
  });

  const onSubmit = handleSubmit((data) => {
    if (selectedEffect?.id) {
      updateMutation.mutate({
        data,
        id: selectedEffect.id,
      });
    } else {
      createMutation.mutate(data);
    }
  });

  return (
    <Card.Root minW="700px">
      <Card.Body>
        <Box onSubmit={onSubmit} as="form">
          <VStack spaceY={4} align="stretch">
            <FieldRoot invalid={!!errors.name} required>
              <FieldLabel htmlFor="description">Name</FieldLabel>
              <Input
                id="name"
                {...register("name", {
                  required: "Name is required",
                })}
                placeholder="Alert on new transactions"
                type="text"
              />
              {errors.name && (
                <FieldErrorText>{errors.name.message}</FieldErrorText>
              )}
            </FieldRoot>

            <FieldRoot invalid={!!errors.frequency_days} required>
              <FieldLabel htmlFor="frequency_days">
                Send most once every X days
              </FieldLabel>
              <Input
                id="frequency_days"
                {...register("frequency_days", {
                  required: "Frequency is required",
                })}
                placeholder="Frequency"
                type="number"
              />
              {errors.frequency_days && (
                <FieldErrorText>{errors.frequency_days.message}</FieldErrorText>
              )}
            </FieldRoot>

            <FieldRoot invalid={!!errors.event_type} required mt={4}>
              <FieldLabel htmlFor="event_type">Event Type</FieldLabel>
              <Controller
                control={control}
                name="event_type"
                render={({ field }) => {
                  const { onChange } = field;
                  return (
                    <SelectRoot
                      id="event_type"
                      defaultValue={[
                        eventTypes.find(
                          (eventType) =>
                            eventType === selectedEffect?.event_type,
                        )!,
                      ]}
                      collection={createListCollection({ items: eventTypes })}
                      onValueChange={(val) => {
                        onChange(val.value[0]);
                      }}
                    >
                      <SelectTrigger>
                        <SelectValueText placeholder="Select an event type" />
                      </SelectTrigger>
                      <SelectContent>
                        {eventTypes.map((eventType) => (
                          <SelectItem key={eventType} item={eventType}>
                            {eventType}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </SelectRoot>
                  );
                }}
              />
              {errors.event_type && (
                <FieldErrorText>{errors.event_type.message}</FieldErrorText>
              )}
            </FieldRoot>

            <FieldRoot invalid={!!errors.condition} required mt={4}>
              <FieldLabel htmlFor="condition">Condition</FieldLabel>
              <Controller
                control={control}
                name="condition"
                render={({ field }) => {
                  const { onChange } = field;
                  return (
                    <SelectRoot
                      id="condition"
                      defaultValue={[
                        conditionTypes.find(
                          (condition) =>
                            condition === selectedEffect?.condition,
                        )!,
                      ]}
                      collection={createListCollection({
                        items: conditionTypes,
                      })}
                      onValueChange={(val) => {
                        onChange(val.value[0]);
                      }}
                    >
                      <SelectTrigger>
                        <SelectValueText placeholder="Select a condition" />
                      </SelectTrigger>
                      <SelectContent>
                        {conditionTypes.map((conditionType) => (
                          <SelectItem key={conditionType} item={conditionType}>
                            {conditionType}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </SelectRoot>
                  );
                }}
              />
              {errors.condition && (
                <FieldErrorText>{errors.condition.message}</FieldErrorText>
              )}
            </FieldRoot>

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
                      step={condition === "amount_over" ? 0.01 : 1}
                      value={
                        condition === "amount_over"
                          ? value?.amount_over || 0
                          : value?.count || 0
                      }
                      onChange={(e) => {
                        const numValue = parseFloat(e.target.value);
                        onChange(
                          condition === "amount_over"
                            ? { amount_over: numValue }
                            : { count: numValue },
                        );
                      }}
                      placeholder={
                        condition === "amount_over"
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
                      condition === "amount_over"
                        ? value?.amount_over
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

            <FieldRoot invalid={!!errors.effect_type} required mt={4}>
              <FieldLabel htmlFor="effect_type">Effect Type</FieldLabel>
              <Controller
                control={control}
                name="effect_type"
                render={({ field }) => {
                  const { onChange } = field;
                  return (
                    <SelectRoot
                      id="effect_type"
                      defaultValue={[
                        effectTypes.find(
                          (effectType) =>
                            effectType === selectedEffect?.effect_type,
                        )!,
                      ]}
                      collection={createListCollection({ items: effectTypes })}
                      onValueChange={(val) => {
                        onChange(val.value[0]);
                      }}
                    >
                      <SelectTrigger>
                        <SelectValueText placeholder="Select an effect type" />
                      </SelectTrigger>
                      <SelectContent>
                        {effectTypes.map((effectType) => (
                          <SelectItem key={effectType} item={effectType}>
                            {effectType}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </SelectRoot>
                  );
                }}
              />
              {errors.effect_type && (
                <FieldErrorText>{errors.effect_type.message}</FieldErrorText>
              )}
            </FieldRoot>

            <FieldRoot invalid={!!errors.subject} required>
              <FieldLabel htmlFor="subject">Subject</FieldLabel>
              <Input
                id="subject"
                {...register("subject", {
                  required: "Subject is required",
                })}
                placeholder="Subject"
                type="text"
              />
              {errors.subject && (
                <FieldErrorText>{errors.subject.message}</FieldErrorText>
              )}
            </FieldRoot>

            <FieldRoot invalid={!!errors.template} required>
              <FieldLabel htmlFor="template">Template</FieldLabel>
              <Textarea
                id="template"
                {...register("template", {
                  required: "Template is required",
                })}
                placeholder="Template"
              />
              {errors.template && (
                <FieldErrorText>{errors.template.message}</FieldErrorText>
              )}
            </FieldRoot>

            <HStack>
              <Button
                type="submit"
                loading={
                  isSubmitting ||
                  createMutation.isPending ||
                  updateMutation.isPending
                }
                mr={3}
              >
                {selectedEffect ? "Update Notification" : "Save Notification"}
              </Button>
            </HStack>
          </VStack>
        </Box>
      </Card.Body>
    </Card.Root>
  );
}
