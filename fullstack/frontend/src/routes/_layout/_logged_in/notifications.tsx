import { createFileRoute } from "@tanstack/react-router";
import { useForm } from "react-hook-form";
import { NoCodeService } from "@/client";
import React, { useEffect, useState } from "react";
import { Box, Button, Spinner, useDisclosure } from "@chakra-ui/react";
import { DumbSelect } from "@/components/ui/dumb-select";
import { useQuery } from "@tanstack/react-query";
import { EffectOut } from "@/client/types.gen";
import Delete, { DeleteableEntity } from "@/components/Common/DeleteAlert";
import { NotificationPreview } from "@/components/Notifications/Preview";
import {
  NotificationFormValues,
  CreateForm,
} from "@/components/Notifications/Builder";

export const Route = createFileRoute("/_layout/_logged_in/notifications")({
  component: UnifiedNotificationInterface,
});

function UnifiedNotificationInterface() {
  const { data: effects } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  const [selectedEffect, setSelectedEffect] = useState<EffectOut | null>(
    effects?.[0] || null,
  );

  const form = useForm<NotificationFormValues>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: selectedEffect
      ? selectedEffectToFormValues(selectedEffect)
      : undefined,
  });

  useEffect(() => {
    if (selectedEffect) {
      form.reset(selectedEffectToFormValues(selectedEffect));
    }
  }, [selectedEffect, form]);

  const { data: effectMappings } = useQuery({
    queryKey: ["effect_mappings"],
    queryFn: () => NoCodeService.getEffectMappings(),
  });

  const deleteModal = useDisclosure();

  if (!effectMappings || !effects) {
    return <Spinner />;
  }

  return (
    <Box display="flex" flexDirection="column" alignItems="flex-start" gap={8}>
      <Box display="flex" gap={2} alignItems="flex-end" maxW="400px">
        <Box>
          <DumbSelect
            selectedOption={selectedEffect}
            setSelectedOption={setSelectedEffect}
            labelExtractor={(effect) => effect.name}
            keyExtractor={(effect) => effect.id.toString()}
            options={effects || []}
            label="Select Notification"
          />
        </Box>
        <NewNotificationButton
          setSelectedEffect={setSelectedEffect}
          resetForm={() => form.reset()}
        />
        <Button onClick={deleteModal.onOpen} variant="ghost" color="red">
          Delete
        </Button>
      </Box>
      <Box display="flex" gap={8}>
        {selectedEffect && effectMappings && (
          <Box flex={2}>
            <CreateForm
              form={form}
              effectMappings={effectMappings}
              selectedEffect={selectedEffect}
              setSelectedEffect={setSelectedEffect}
            />
          </Box>
        )}
        <Box flex={3}>
          <NotificationPreview form={form} />
        </Box>
        <Delete
          type="notification"
          isOpen={deleteModal.open}
          onClose={deleteModal.onClose}
          entity={selectedEffect as DeleteableEntity}
        />
      </Box>
    </Box>
  );
}

function NewNotificationButton({
  setSelectedEffect,
  resetForm,
}: {
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
  resetForm: () => void;
}) {
  return (
    <Button
      onClick={() => {
        setSelectedEffect({
          id: -1,
          name: "",
          active: true,
          editable: true,
          config: {
            template: "",
            subject: "",
            frequency_days: 0,
          },
          event_type: "new_transaction",
          effect_type: "email",
          condition: "amount_over",
          conditional_parameters: {},
        });
        resetForm();
      }}
    >
      New Notification
    </Button>
  );
}

function selectedEffectToFormValues(selectedEffect: EffectOut) {
  return {
    name: selectedEffect.name,
    active: selectedEffect.active,
    template: selectedEffect.config.template,
    subject: selectedEffect.config.subject,
    effect_type: selectedEffect.effect_type,
    event_type: selectedEffect.event_type,
    frequency_days: selectedEffect.config.frequency_days,
    condition: selectedEffect.condition,
    conditional_parameters: selectedEffect.conditional_parameters,
  };
}