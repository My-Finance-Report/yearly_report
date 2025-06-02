import { createFileRoute } from "@tanstack/react-router";
import { NoCodeService } from "@/client";
import React, { useState } from "react";
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
  const [selectedEffect, setSelectedEffect] = useState<EffectOut | null>(null);
  const [formValues, setFormValues] = useState<NotificationFormValues | null>(
    null,
  );

  const { data: effectMappings } = useQuery({
    queryKey: ["effect_mappings"],
    queryFn: () => NoCodeService.getEffectMappings(),
  });

  const deleteModal = useDisclosure();

  return (
    <Box display="flex" flexDirection="column" alignItems="center" gap={8}>
      <Box display="flex" gap={2} alignItems="flex-end" maxW="400px">
        <EffectSelector
          setSelectedEffect={setSelectedEffect}
          selectedEffect={selectedEffect}
        />
        <NewNotificationButton
          setSelectedEffect={setSelectedEffect}
          setFormValues={setFormValues}
        />
      </Box>
      <Box display="flex" gap={8}>
        {selectedEffect && effectMappings && (
          <Box flex={2}>
            <CreateForm
              effectMappings={effectMappings}
              selectedEffect={selectedEffect}
              setFormValues={setFormValues}
            />
          </Box>
        )}
        <Box flex={3}>
          <NotificationPreview formValues={formValues} />
        </Box>

        {selectedEffect?.id && selectedEffect.id !== undefined && (
          <Delete
            type="notification"
            isOpen={deleteModal.open}
            onClose={deleteModal.onClose}
            entity={selectedEffect as DeleteableEntity}
          />
        )}
      </Box>
    </Box>
  );
}

interface EffectSelectorProps {
  selectedEffect: EffectOut | null;
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
}

function EffectSelector({
  selectedEffect,
  setSelectedEffect,
}: EffectSelectorProps) {
  const { data: effects, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  if (isLoading) {
    return <Spinner />;
  }

  return (
    <Box>
      <DumbSelect
        selectedOption={selectedEffect}
        setSelectedOption={setSelectedEffect}
        labelExtractor={(effect) => effect.name}
        keyExtractor={(effect) => String(effect.id)}
        options={effects || []}
        label="Select Notification"
      />
    </Box>
  );
}

function NewNotificationButton({
  setSelectedEffect,
  setFormValues,
}: {
  setSelectedEffect: React.Dispatch<React.SetStateAction<EffectOut | null>>;
  setFormValues: React.Dispatch<
    React.SetStateAction<NotificationFormValues | null>
  >;
}) {
  return (
    <Button
      onClick={() => {
        setSelectedEffect({
          name: "",
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
        setFormValues(null);
      }}
    >
      New Notification
    </Button>
  );
}
