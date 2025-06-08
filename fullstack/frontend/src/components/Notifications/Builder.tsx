import { NoCodeService } from "@/client";
import { Conditions } from "./Conditions/Conditions";
import {
  DumbNumberField,
  DumbTextField,
  TemplateEditor,
} from "../ui/dumb/form/value";
import { DumbFormSelect } from "../ui/dumb/form/select";
import { Box, Button, Stack, Badge, Switch, Card } from "@chakra-ui/react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  EffectOut,
  EventType,
  EffectType,
  EffectConditionals,
  EffectCreate,
  EffectUpdate,
  ConditionalParameters,
  EffectMappings,
} from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";
import { HiCheck, HiX } from "react-icons/hi";
import { useForm } from "react-hook-form";

export interface NotificationFormValues {
  id?: number;
  name: string;
  active: boolean;
  template: string;
  subject: string;
  effect_type: EffectType;
  event_type: EventType;
  frequency_days: number;
  condition: EffectConditionals;
  conditional_parameters: ConditionalParameters;
}

interface CreateFormProps {
  form: ReturnType<typeof useForm<NotificationFormValues>>;
  selectedEffect: EffectOut;
  effectMappings: EffectMappings;
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
}

const EVENT_TYPES: Record<EventType, string> = {
  new_transaction: "New Transaction",
  new_account_linked: "New Account Linked",
  account_deactivated: "Account Deactivated",
  budget_threshold_exceeded: "Budget Threshold Exceeded",
};

const EFFECT_TYPES: Record<EffectType, string> = {
  email: "Email",
  in_app: "In App",
};

function determineConditionsForEventType(
  eventType: EventType,
): EffectConditionals[] {
  if (eventType === "new_transaction") {
    return ["amount_over", "count_of_transactions"];
  }
  if (eventType === "budget_threshold_exceeded") {
    return ["amount_over"];
  }
  return [];
}

function supportsFrequency(eventType: EventType) {
  return eventType === "new_transaction";
}

export function CreateForm({
  form,
  selectedEffect,
  effectMappings,
  setSelectedEffect,
}: CreateFormProps) {
  const showToast = useCustomToast();
  const queryClient = useQueryClient();

  const {
    register,
    handleSubmit,
    control,
    formState: { errors, isSubmitting, isDirty },
  } = form;

  console.log(effectMappings);

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
    onSuccess: (response) => {
      showToast(
        "Notification updated",
        "Notification updated successfully.",
        "success",
      );
      setSelectedEffect(response);
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
        active: data.active,
        frequency_days: data.frequency_days,
        template: data.template,
        subject: data.subject,
        condition: data.condition,
        conditional_parameters: data.conditional_parameters,
      };

      return NoCodeService.createEffect({ requestBody: body });
    },
    onSuccess: (data) => {
      showToast(
        "Notification created",
        "Notification created successfully.",
        "success",
      );
      queryClient.invalidateQueries({ queryKey: ["effects"] });
      setSelectedEffect(data);
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
    <Card.Root w="full" minW={{ base: "full" }} maxW="900px">
      <Card.Header>
        <Stack
          direction={{ base: "column", sm: "row" }}
          gap={4}
          w="full"
          align={{ base: "stretch", sm: "center" }}
          justify="space-between"
        >
          <Stack direction="column" gap={2}>
            <Card.Title>{selectedEffect?.name}</Card.Title>
            {!selectedEffect?.editable && (
              <Badge colorPalette="yellow" size="md">
                This notification is not editable. You can turn it on or off.
              </Badge>
            )}
            {isDirty && (
              <Badge colorPalette="yellow" size="md">
                Unsaved changes
              </Badge>
            )}
          </Stack>
          <ToggleActive
            selectedEffect={selectedEffect}
            setSelectedEffect={setSelectedEffect}
          />
        </Stack>
      </Card.Header>
      <Card.Body>
        <Box onSubmit={onSubmit} as="form">
          <Stack gap={6} align="stretch">
            <Stack direction={{ base: "column", md: "row" }} gap={4}>
              <Box flex={1}>
                <DumbTextField
                  name="name"
                  label="Name of notification"
                  register={register}
                  errors={errors}
                  disabled={!selectedEffect?.editable}
                />
              </Box>
              <Box flex={1}>
                <DumbFormSelect
                  control={control}
                  errors={errors}
                  name="event_type"
                  label="Event"
                  options={Object.keys(EVENT_TYPES).map(
                    (eventType) => eventType as EventType,
                  )}
                  labelExtractor={(eventType) => EVENT_TYPES[eventType]}
                  keyExtractor={(eventType) => eventType}
                  disabled={!selectedEffect?.editable}
                />
              </Box>
            </Stack>

            {supportsFrequency(form.getValues("event_type")) && (
              <Box maxW="200px">
                <DumbNumberField
                  name="frequency_days"
                  label="Frequency (days)"
                  register={register}
                  errors={errors}
                  disabled={!selectedEffect?.editable}
                />
              </Box>
            )}

            <Conditions
              control={control}
              errors={errors}
              form={form}
              disabled={!selectedEffect?.editable}
              supported_conditional_parameters={determineConditionsForEventType(
                form.getValues("event_type"),
              )}
            />

            <Stack direction={{ base: "column", md: "row" }} gap={4} align="start">
                <DumbFormSelect
                  control={control}
                  errors={errors}
                  name="effect_type"
                  label="Notification Type"
                  disabled={!selectedEffect?.editable}
                  options={Object.keys(EFFECT_TYPES).map(
                    (effectType) => effectType as EffectType,
                  )}
                  labelExtractor={(effectType) => EFFECT_TYPES[effectType]}
                  keyExtractor={(effectType) => effectType}
                />
                <DumbTextField
                  name="subject"
                  label="Subject"
                  register={register}
                  disabled={!selectedEffect?.editable}
                  errors={errors}
                />
            </Stack>

            <Box w="100%" minH={{ base: "200px", md: "300px" }}>
              <TemplateEditor
                name="template"
                availableVariables={
                  effectMappings.variables[form.getValues("event_type")] || []
                }
                label="Template"
                register={register}
                errors={errors}
                setValue={form.setValue}
                disabled={!selectedEffect?.editable}
              />
            </Box>

            <Box pt={4}>
              <Button
                type="submit"
                loading={
                  isSubmitting ||
                  createMutation.isPending ||
                  updateMutation.isPending
                }
                colorScheme="blue"
                size="md"
              >
                {selectedEffect ? "Update Notification" : "Save Notification"}
              </Button>
            </Box>
          </Stack>
        </Box>
      </Card.Body>
    </Card.Root>
  );
}

function ToggleActive({
  selectedEffect,
  setSelectedEffect,
}: {
  selectedEffect: EffectOut;
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
}) {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();

  const mutation = useMutation({
    mutationFn: () => {
      if (!selectedEffect.id) {
        throw new Error("Effect ID is required");
      }
      return NoCodeService.toggeEffectActivity({
        effectId: selectedEffect.id,
        isActive: !selectedEffect.active,
      });
    },
    onSuccess: () => {
      showToast(
        "Notification updated",
        "Notification updated successfully.",
        "success",
      );
      setSelectedEffect({
        ...selectedEffect,
        active: !selectedEffect.active,
      });
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast("Error updating notification", error.message, "error");
    },
  });

  return (
    <Badge
      flex={"row"}
      justifyContent={"space-between"}
      size="md"
      p={5}
      colorPalette={selectedEffect.active ? "green" : "red"}
    >
      {selectedEffect.active ? "Active" : "Inactive"}
      <Switch.Root
        variant="solid"
        size="lg"
        checked={selectedEffect.active}
        onCheckedChange={() => mutation.mutate()}
      >
        <Switch.HiddenInput />
        <Switch.Control>
          <Switch.Thumb>
            <Switch.ThumbIndicator fallback={<HiX color="black" />}>
              <HiCheck />
            </Switch.ThumbIndicator>
          </Switch.Thumb>
        </Switch.Control>
        <Switch.Label />
      </Switch.Root>
    </Badge>
  );
}
