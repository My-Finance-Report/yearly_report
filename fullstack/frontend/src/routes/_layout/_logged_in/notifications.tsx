import { createFileRoute } from "@tanstack/react-router";
import { NoCodeService } from "@/client";
import React, { useState, useEffect } from "react";
import {
  Box,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
  Button,
  Heading,
  HStack,
  Input,
  Spinner,
  FieldRoot,
  FieldLabel,
  Textarea,
  Card,
  VStack,
  useDisclosure,
  FieldErrorText,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import {
  EffectOut,
  Email,
  EventType,
  EffectType,
  EffectConditionals,
  EffectCreate,
  EffectUpdate,
} from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";
import Delete from "@/components/Common/DeleteAlert";
import { Controller, useForm, UseFormReturn } from "react-hook-form";

export const Route = createFileRoute("/_layout/_logged_in/notifications")({
  component: NotificationsPage,
});

function NotificationsPage() {
  return (
    <Box p={4}>
      <UnifiedNotificationInterface />
    </Box>
  );
}

interface NotificationFormValues {
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

function UnifiedNotificationInterface() {
  const [previewData, setPreviewData] = useState<Email | null>(null);
  const [selectedEffect, setSelectedEffect] = useState<EffectOut | null>(null);

  const deleteModal = useDisclosure();

  const form = useForm<NotificationFormValues>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      name: selectedEffect?.name || "",
      template: selectedEffect?.config.template || "",
      subject: selectedEffect?.config.subject || "",
      effect_type: selectedEffect?.effect_type || "email",
      event_type: selectedEffect?.event_type || "new_transaction",
      frequency_days: selectedEffect?.config.frequency_days || 1,
      condition: selectedEffect?.condition || "count_of_transactions",
      conditional_parameters: selectedEffect?.conditional_parameters,
    },
  });

  const formValues = form.watch();

  // Fetch all notification effects
  // Preview query
  const { data, refetch } = useQuery({
    queryKey: ["previewNotification", formValues.template, formValues.subject],
    queryFn: async () => {
      return NoCodeService.previewNotification({
        template: formValues.template,
        subject: formValues.subject,
        numTransactions: 3,
        accountName: "Test Account",
      });
    },
    enabled: !!formValues.template && !!formValues.subject,
  });

  useEffect(() => {
    refetch();
    if (!data) return;
    setPreviewData(data);
  }, [formValues]);

  return (
    <>
      <EffectSelector setSelectedEffect={setSelectedEffect} />
      <HStack>
        <CreateForm form={form} selectedEffect={selectedEffect} />
        <NotificationPreview
          subject={previewData?.subject}
          html={previewData?.html}
        />

        {selectedEffect && (
          <Delete
            type="notification"
            isOpen={deleteModal.open}
            onClose={deleteModal.onClose}
            entity={selectedEffect}
          />
        )}
      </HStack>
    </>
  );
}

interface EffectSelectorProps {
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
}

function EffectSelector({ setSelectedEffect }: EffectSelectorProps) {
  const { data: effects, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  const onChange = (effectName: string) => {
    const effect = effects?.find((effect) => effect.name === effectName);
    setSelectedEffect(effect || null);
  };

  if (isLoading) {
    return <Spinner />;
  }

  return (
    <SelectRoot
      id="effect"
      collection={createListCollection({
        items: effects?.map((effect) => effect.name) || [],
      })}
      value={[effects?.[0]?.name || ""]}
      onValueChange={(val) => {
        onChange(val.value[0]);
      }}
    >
      <SelectTrigger>
        <SelectValueText placeholder="Select an event type" />
      </SelectTrigger>
      <SelectContent>
        {effects?.map((effect) => (
          <SelectItem key={effect.name} item={effect.name}>
            {effect.name}
          </SelectItem>
        ))}
      </SelectContent>
    </SelectRoot>
  );
}

interface CreateFormProps {
  form: UseFormReturn<NotificationFormValues, null, NotificationFormValues>;
  selectedEffect: EffectOut | null;
}

function CreateForm({ form, selectedEffect }: CreateFormProps) {
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
    <Card.Root minW="400px">
      <Card.Header>
        <HStack justifyContent="space-between">
          <Heading size="md">Create Notification</Heading>
        </HStack>
      </Card.Header>
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
                Conditional Parameters
              </FieldLabel>
              <Controller
                control={control}
                name="conditional_parameters"
                render={({ field }) => {
                  const { onChange } = field;
                  return (
                    <Textarea
                      id="conditional_parameters"
                      onChange={(e) => {
                        try {
                          const parsedValue = JSON.parse(e.target.value);
                          onChange(parsedValue);
                        } catch {
                          e.target.dataset.jsonError = "true";
                        }
                      }}
                      placeholder='{"count": 1}'
                      rows={4}
                    />
                  );
                }}
                rules={{
                  required: "Conditional parameters are required",
                  validate: (value) => {
                    return (
                      (typeof value === "object" && value !== null) ||
                      "Must be a valid JSON object"
                    );
                  },
                }}
              />
              {errors.conditional_parameters?.message && (
                <FieldErrorText>
                  {errors.conditional_parameters?.message.message}
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
              <FieldLabel htmlFor="subject">Suject</FieldLabel>
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

interface NotificationPreviewProps {
  html?: string;
  subject?: string;
}

function NotificationPreview({ html, subject }: NotificationPreviewProps) {
  if (!html || !subject) {
    return null;
  }

  return (
    <Card.Root className="border">
      <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium">
        {subject}
      </Card.Header>
      <Card.Body className="p-0 overflow-hidden">
        <div dangerouslySetInnerHTML={{ __html: html }} />
      </Card.Body>
    </Card.Root>
  );
}
