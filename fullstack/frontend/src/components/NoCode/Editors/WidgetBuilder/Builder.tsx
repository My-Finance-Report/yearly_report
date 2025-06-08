import {
  NoCodeParameterCreate,
  NoCodeService,
  NoCodeWidgetCreate,
} from "@/client";
import { DumbFormSelect } from "@/components/ui/dumb/form/select";
import { DumbTextField } from "@/components/ui/dumb/form/value";
import {
  Button,
  Dialog,
  Portal,
  VStack,
  HStack,
  Text,
  Box,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { useRef } from "react";
import { useForm } from "react-hook-form";

export function WidgetBuilder({
  isOpen,
  onClose,
}: {
  isOpen: boolean;
  onClose: () => void;
}) {
  const form = useForm<NoCodeWidgetCreate>();

  const handleSubmit = (data: NoCodeWidgetCreate) => {
    NoCodeService.createWidget({
      requestBody: data,
    });
    onClose();
  };

  const cancelRef = useRef<HTMLButtonElement | null>(null);

  return (
    <Dialog.Root open={isOpen} onExitComplete={onClose}>
      <Portal>
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content onSubmit={form.handleSubmit(handleSubmit)}>
            <Dialog.Header>Build Widget</Dialog.Header>
            <Dialog.Body>
              <PipelineEditor form={form} />
              <WidgetMiscEditor form={form} />
            </Dialog.Body>
            <Dialog.Footer gap={3}>
              <Button ref={cancelRef} onClick={onClose}>
                Cancel
              </Button>
              <Button colorScheme="blue" type="submit">
                Build
              </Button>
            </Dialog.Footer>
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </Dialog.Root>
  );
}

function WidgetMiscEditor({
  form,
}: {
  form: ReturnType<typeof useForm<NoCodeWidgetCreate>>;
}) {
  return (
    <VStack>
      <DumbTextField
        name="name"
        label="Name of notification"
        register={form.register}
        errors={form.formState.errors}
      />
      <DumbFormSelect
        name="type"
        label="Widget Type"
        control={form.control}
        errors={form.formState.errors}
        options={["chart", "table"]}
        labelExtractor={(type) => type}
        keyExtractor={(type) => type}
      />
    </VStack>
  );
}

function PipelineEditor({
  form,
}: {
  form: ReturnType<typeof useForm<NoCodeWidgetCreate>>;
}) {
  const tools = useQuery({
    queryKey: ["no_code_tools"],
    queryFn: () => NoCodeService.getNoCodeTools(),
  });

  const pipelineSteps = form.watch("pipeline") || [];

  return (
    <VStack align="stretch" width="100%">
      <Text fontWeight="medium">Pipeline Steps</Text>

      {pipelineSteps.map((_, index) => (
        <Box key={index} p={4} borderWidth={1} borderRadius="md">
          <HStack spaceX={4} align="stretch">
            <DumbFormSelect
              name={`pipeline.${index}.tool`}
              label={`Step ${index + 1}`}
              control={form.control}
              errors={form.formState.errors}
              options={tools.data || []}
              labelExtractor={(tool) => tool.name}
              keyExtractor={(tool) => tool.tool}
            />
            <ParameterList form={form} stepIndex={index} />
          </HStack>
        </Box>
      ))}

      <Button
        onClick={() => {
          const currentPipeline = form.getValues("pipeline") || [];
          form.setValue("pipeline", [
            ...currentPipeline,
            { tool: "", parameters: [] },
          ]);
        }}
        size="sm"
        variant="outline"
      >
        Add Step
      </Button>
    </VStack>
  );
}

function ParameterList({
  form,
  stepIndex,
}: {
  form: ReturnType<typeof useForm<NoCodeWidgetCreate>>;
  stepIndex: number;
}) {
  const tools = useQuery({
    queryKey: ["no_code_tools"],
    queryFn: () => NoCodeService.getNoCodeTools(),
  });

  const selectedTool = form.watch(`pipeline.${stepIndex}.tool`);
  const tool = tools.data?.find((t) => t.tool === selectedTool);

  if (!tool?.parameters?.length) return null;

  return (
    <VStack spaceX={3} align="stretch">
      {tool.parameters.map((param, paramIndex) => (
        <ParameterEditor
          key={param.name}
          param={param}
          paramIndex={paramIndex}
          form={form}
          stepIndex={stepIndex}
        />
      ))}
    </VStack>
  );
}

function ParameterEditor({
  param,
  paramIndex,
  form,
  stepIndex,
}: {
  param: NoCodeParameterCreate;
  paramIndex: number;
  form: ReturnType<typeof useForm<NoCodeWidgetCreate>>;
  stepIndex: number;
}) {
  switch (param.type) {
    case "int":
    case "float":
      return (
        <DumbTextField
          name={`pipeline.${stepIndex}.parameters.${paramIndex}.value`}
          label={param.label || param.name}
          register={form.register}
          errors={form.formState.errors}
        />
      );
    case "string":
      return (
        <DumbTextField
          name={`pipeline.${stepIndex}.parameters.${paramIndex}.value`}
          label={param.label || param.name}
          register={form.register}
          errors={form.formState.errors}
        />
      );
    case "select":
      return (
        <DumbFormSelect
          name={`pipeline.${stepIndex}.parameters.${paramIndex}.value`}
          label={param.label || param.name}
          control={form.control}
          errors={form.formState.errors}
          options={[]}
          labelExtractor={(opt) => opt}
          keyExtractor={(opt) => opt}
        />
      );
    default:
      return (
        <Box p={2} bg="gray.100" borderRadius="md">
          <Text color="gray.600">Unsupported parameter type: {param.type}</Text>
        </Box>
      );
  }
}
