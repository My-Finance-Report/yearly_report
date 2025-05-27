import {
  type ApiError,
  NoCodeService,
  NoCodeWidgetIn_Output,
  NoCodeWidgetUpdate,
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

export function EditModal({
  widget,
  editMode,
  children,
}: {
  widget: NoCodeWidgetIn_Output;
  editMode: boolean;
  children: React.ReactNode;
}) {
  const showToast = useCustomToast();
  const editDisclosure = useDisclosure();
  const queryClient = useQueryClient();

  const {
    register,
    handleSubmit,
    reset,
    formState: { errors, isSubmitting },
  } = useForm<NoCodeWidgetUpdate>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: widget,
  });

  const mutation = useMutation({
    mutationFn: (widgetUpdate: NoCodeWidgetUpdate) =>
      NoCodeService.updateWidget({
        widgetId: widget.id,
        requestBody: widgetUpdate,
      }),
    onSuccess: () => {
      showToast("Updated Widget", "Changes have been saved", "success");
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

  const onSubmit: SubmitHandler<NoCodeWidgetUpdate> = async (data) => {
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
          colorPalette={"green"}
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
              <Dialog.Title>Edit {widget.name}</Dialog.Title>
              <Dialog.CloseTrigger onClick={editDisclosure.onClose} asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
            <Dialog.Body>
              <Field.Root invalid={!!errors.name} required>
                <Field.Label htmlFor="name">Name</Field.Label>
                <Field.Label htmlFor="name" fontSize={"xs"}>
                  Blank for no name
                </Field.Label>
                <Input
                  id="name"
                  {...register("name", {
                    required: false,
                  })}
                  placeholder="Name"
                  required={false}
                  type="text"
                />
                {errors.name && (
                  <Field.ErrorText>{errors.name.message}</Field.ErrorText>
                )}
              </Field.Root>

              <Field.Root invalid={!!errors.row} required>
                <Field.Label htmlFor="row">Row</Field.Label>
                <Input
                  id="name"
                  {...register("row", {
                    required: "Row is required",
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
                  id="name"
                  {...register("row_span", {
                    required: "Row Span is required",
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
