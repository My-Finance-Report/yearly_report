import {
  type ApiError,
  NoCodeService,
  Parameter_Output,
  NoCodeParameterUpdate,
} from "@/client";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  Button,
  CloseButton,
  Dialog,
  Field,
  HStack,
  useDisclosure,
  Input,
  Box,
} from "@chakra-ui/react";
import useCustomToast from "@/hooks/useCustomToast";
import { SubmitHandler, useForm } from "react-hook-form";
import { FaEllipsisH } from "react-icons/fa";

export function EditParameterModal({
  param,
  editMode,
  children,
}: {
  param: Parameter_Output;
  editMode: boolean;
  children: React.ReactNode;
}) {
  const showToast = useCustomToast();
  const editDisclosure = useDisclosure();
  const queryClient = useQueryClient();

  // Extract display info values for the form
  const defaultValues: NoCodeParameterUpdate = {
    label: param.label || "",
    row: param.display_info?.row || 1,
    col: param.display_info?.col || 1,
    row_span: param.display_info?.row_span || 1,
    col_span: param.display_info?.col_span || 1,
  };

  const {
    register,
    handleSubmit,
    reset,
    formState: { errors, isSubmitting },
  } = useForm<NoCodeParameterUpdate>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues,
  });

  const mutation = useMutation({
    mutationFn: (paramUpdate: NoCodeParameterUpdate) =>
      NoCodeService.updateParameter({
        parameterId: param.id,
        requestBody: paramUpdate,
      }),
    onSuccess: () => {
      showToast("Updated Parameter", "Changes have been saved", "success");
    },
    onError: (err: ApiError) => {
      console.log("err", err.message);
    },
    onSettled: () => {
      queryClient.invalidateQueries({
        queryKey: ["accounts-no-code"],
      });
    },
  });

  const onSubmit: SubmitHandler<NoCodeParameterUpdate> = async (data) => {
    mutation.mutate(data);
    editDisclosure.onClose();
  };

  const onCancel = () => {
    reset();
    editDisclosure.onClose();
  };

  if (!editMode) {
    return children;
  }

  return (
    <>
      <Box position="relative">
        <Button
          position="absolute"
          right={4}
          colorPalette={"blue"}
          top={4}
          borderWidth={1}
          cursor={"pointer"}
          borderRadius={"md"}
          p={3}
          zIndex={100}
          variant={"subtle"}
          size="xs"
          onClick={() => {
            editDisclosure.onOpen();
          }}
        >
          <FaEllipsisH />
        </Button>

        {children}
      </Box>
      <Dialog.Root
        open={editDisclosure.open}
        onExitComplete={editDisclosure.onClose}
        onInteractOutside={editDisclosure.onClose}
        motionPreset={"slide-in-bottom"}
      >
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content onSubmit={handleSubmit(onSubmit)} as="form">
            <Dialog.Header>
              <Dialog.Title>Edit Parameter: {param.name}</Dialog.Title>
              <Dialog.CloseTrigger onClick={editDisclosure.onClose} asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
            <Dialog.Body>
              <Field.Root invalid={!!errors.label}>
                <Field.Label htmlFor="label">Label</Field.Label>
                <Field.Label htmlFor="label" fontSize={"xs"}>
                  Display label for the parameter
                </Field.Label>
                <Input
                  id="label"
                  {...register("label")}
                  placeholder="Label"
                  type="text"
                />
                {errors.label && (
                  <Field.ErrorText>{errors.label.message}</Field.ErrorText>
                )}
              </Field.Root>

              <Field.Root invalid={!!errors.row} required>
                <Field.Label htmlFor="row">Row</Field.Label>
                <Input
                  id="row"
                  {...register("row", {
                    required: "Row is required",
                    valueAsNumber: true,
                  })}
                  placeholder="Row"
                  type="number"
                />
                {errors.row && (
                  <Field.ErrorText>{errors.row.message}</Field.ErrorText>
                )}
              </Field.Root>

              <Field.Root invalid={!!errors.row_span} required>
                <Field.Label htmlFor="row_span">Row Span</Field.Label>
                <Input
                  id="row_span"
                  {...register("row_span", {
                    required: "Row Span is required",
                    valueAsNumber: true,
                  })}
                  placeholder="Row Span"
                  type="number"
                />
                {errors.row_span && (
                  <Field.ErrorText>{errors.row_span.message}</Field.ErrorText>
                )}
              </Field.Root>

              <Field.Root invalid={!!errors.col} required>
                <Field.Label htmlFor="col">Column</Field.Label>
                <Input
                  id="col"
                  {...register("col", {
                    required: "Col is required",
                    valueAsNumber: true,
                  })}
                  placeholder="Col"
                  type="number"
                />
                {errors.col && (
                  <Field.ErrorText>{errors.col.message}</Field.ErrorText>
                )}
              </Field.Root>

              <Field.Root invalid={!!errors.col_span} required>
                <Field.Label htmlFor="col_span">Column Span</Field.Label>
                <Input
                  id="col_span"
                  {...register("col_span", {
                    required: "Col Span is required",
                    valueAsNumber: true,
                  })}
                  placeholder="Col Span"
                  type="number"
                />
                {errors.col_span && (
                  <Field.ErrorText>{errors.col_span.message}</Field.ErrorText>
                )}
              </Field.Root>
            </Dialog.Body>

            <Dialog.Footer gap={3}>
              <HStack>
                <Button variant="outline" onClick={onCancel} title="Cancel">
                  Cancel
                </Button>
                <Button type="submit" loading={isSubmitting}>
                  Save
                </Button>
              </HStack>
            </Dialog.Footer>
          </Dialog.Content>
        </Dialog.Positioner>
      </Dialog.Root>
    </>
  );
}
