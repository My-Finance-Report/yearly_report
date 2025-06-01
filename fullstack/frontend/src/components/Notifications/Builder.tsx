import { NoCodeService } from "@/client";
import { useEffect } from "react";
import { Conditions } from "./Conditions/Conditions";
import {
  DumbNumberField,
  DumbTextField,
  DumbTextareaField,
} from "../ui/dumb/form/value";
import { DumbFormSelect } from "../ui/dumb/form/select";
import { Box, Button, HStack, Card, VStack } from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  EffectOut,
  EventType,
  EffectType,
  EffectConditionals,
  EffectCreate,
  EffectUpdate,
  ConditionalParameters,
} from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";
import { useForm } from "react-hook-form";

export interface NotificationFormValues {
  id?: number;
  name: string;
  template: string;
  subject: string;
  effect_type: EffectType;
  event_type: EventType;
  frequency_days: number;
  condition: EffectConditionals;
  conditional_parameters: ConditionalParameters;
}

interface CreateFormProps {
  selectedEffect: EffectOut;
  setFormValues: (values: NotificationFormValues) => void;
}

function determineConditionsForEventType(
  eventType: EventType,
): EffectConditionals[] {
  if (eventType === "new_transaction") {
    return ["amount_over", "count_of_transactions"];
  }
  return [];
}

function supportsFrequency(eventType: EventType) {
  return eventType === "new_transaction";
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

  const eventTypes: Record<EventType, string> = {
    new_transaction: "New Transaction",
    new_account_linked: "New Account Linked",
    account_deactivated: "Account Deactivated",
  };

  const effectTypes: Record<EffectType, string> = {
    email: "Email",
    in_app: "In App",
  };

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
        conditional_parameters: data.conditional_parameters,
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
            <DumbTextField
              name="name"
              label="Name of notification"
              register={register}
              errors={errors}
            />
            <DumbFormSelect
              control={control}
              errors={errors}
              name="event_type"
              label="Event"
              options={Object.entries(eventTypes).map(([key, value]) => ({
                value: key,
                label: value,
              }))}
              labelExtractor={(eventType) => eventType.label}
              keyExtractor={(eventType) => eventType.value}
            />
            {supportsFrequency(form.getValues("event_type")) && (
              <DumbNumberField
                name="frequency_days"
                label="Frequency (days)"
                register={register}
                errors={errors}
              />
            )}
            <Conditions
              control={control}
              errors={errors}
              form={form}
              supported_conditional_parameters={determineConditionsForEventType(
                form.getValues("event_type"),
              )}
            />

            <DumbFormSelect
              control={control}
              errors={errors}
              name="effect_type"
              label="Notification Type"
              options={Object.keys(effectTypes).map(
                (effectType) => effectType as EffectType,
              )}
              labelExtractor={(effectType) => effectTypes[effectType]}
              keyExtractor={(effectType) => effectType}
            />
            <DumbTextField
              name="subject"
              label="Subject"
              register={register}
              errors={errors}
            />
            <DumbTextareaField
              name="template"
              label="Template"
              register={register}
              errors={errors}
            />
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
