import { createFileRoute } from "@tanstack/react-router";
import { useForm } from "react-hook-form";
import { NoCodeService } from "@/client";
import React, { useEffect, useState } from "react";
import { Box, Button, useDisclosure, Stack } from "@chakra-ui/react";
import { DumbSelect } from "@/components/ui/dumb-select";
import { useQuery } from "@tanstack/react-query";
import { EffectOut } from "@/client/types.gen";
import Delete, { DeleteableEntity } from "@/components/Common/DeleteAlert";
import { NotificationPreview } from "@/components/Notifications/Preview";
import {
  NotificationFormValues,
  CreateForm,
} from "@/components/Notifications/Builder";
import PageLoader from "@/components/Common/PageLoader";
import { useIsMobile } from "@/hooks/useIsMobile";

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

  const isMobile = useIsMobile();

  const deleteModal = useDisclosure();

  if (!effectMappings || !effects) {
    return <PageLoader />;
  }


  return (
    <Box
      display="flex"
      flexDirection="column"
      gap={8}
      w="full"
      px={isMobile ? 4 : 24}
      maxW="100vw"
      overflowX="hidden"
    >
      <Stack
        direction={{ base: "column", sm: "row" }}
        gap={4}
        align={{ base: "stretch", sm: "center" }}
        justify="space-between"
        w="full"
      >
        <Box flex={{ base: "none", sm: 1 }} w="full" maxW={{ sm: "400px" }}>
          <DumbSelect
            selectedOption={selectedEffect}
            setSelectedOption={setSelectedEffect}
            labelExtractor={(effect) => effect.name}
            keyExtractor={(effect) => effect.id.toString()}
            options={effects || []}
            label="Select Notification"
          />
        </Box>
        <Stack
          direction={{ base: "column", sm: "row" }}
          gap={2}
          w={{ base: "full", sm: "auto" }}
        >
          <NewNotificationButton
            setSelectedEffect={setSelectedEffect}
            resetForm={() => form.reset()}
          />
          <Button
            onClick={deleteModal.onOpen}
            variant="ghost"
            colorScheme="red"
            w={{ base: "full", sm: "auto" }}
          >
            Delete
          </Button>
        </Stack>
      </Stack>

      <Stack
        direction={{ base: "column", lg: "row" }}
        gap={{ base: 6, lg: 8 }}
        w="full"
        align="flex-start"
      >
        {selectedEffect && effectMappings && (
          <Box w="full" minW={0}>
            <CreateForm
              form={form}
              effectMappings={effectMappings}
              selectedEffect={selectedEffect}
              setSelectedEffect={setSelectedEffect}
            />
          </Box>
        )}
        <Box w="full" minW={0}>
          <NotificationPreview form={form} />
        </Box>
        <Delete
          type="notification"
          isOpen={deleteModal.open}
          onClose={deleteModal.onClose}
          entity={selectedEffect as DeleteableEntity}
        />
      </Stack>
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
