import {
  NoCodeCanvasOut,
  NoCodeParameterUpdate,
  NoCodeService,
  NoCodeWidgetOut,
  Parameter_Output,
} from "@/client";
import { DndContext, DragEndEvent, useDraggable } from "@dnd-kit/core";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import useCustomToast from "@/hooks/useCustomToast";
import { ReactNode } from "react";

export function DraggableParameter({
  param,
  editMode,
  children,
}: {
  param: Parameter_Output;
  editMode: boolean;
  children: React.ReactNode;
}) {
  const toast = useCustomToast();
  const queryClient = useQueryClient();

  const parameterMutation = useMutation({
    mutationFn: ({
      param,
      paramId,
    }: {
      param: NoCodeParameterUpdate;
      paramId: number;
    }) =>
      NoCodeService.updateParameter({
        parameterId: paramId,
        requestBody: param,
      }),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["accounts-no-code"] });
      toast("Updated Parameter", "Changes have been saved", "success");
    },
    onError: () => {},
  });

  function updateParameterPosition(
    param: Parameter_Output,
    rowDelta: number,
    colDelta: number,
  ) {
    if (!param) return;

    const newRow = Math.max(param.display_info!.row + rowDelta, 1);
    const newCol = Math.min(
      Math.max(param.display_info!.col + colDelta, 1),
      12,
    );

    parameterMutation.mutate({
      paramId: param.id,
      param: {
        label: param.label || "",
        row: newRow,
        col: newCol,
        row_span: param.display_info?.row_span || 1,
        col_span: param.display_info?.col_span || 1,
      },
    });
  }

  const callable = (movedRows: number, movedCols: number) =>
    updateParameterPosition(param, movedRows, movedCols);

  const { attributes, listeners, setNodeRef, transform } = useDraggable({
    id: `param-${param.id}`,
    data: { callable },
    disabled: !editMode,
  });

  const style = {
    transform: transform
      ? `translate3d(${transform.x}px, ${transform.y}px, 0)`
      : undefined,
    cursor: "move",
  };

  const props = editMode
    ? {
        ref: setNodeRef,
        style: style,
        ...listeners,
        ...attributes,
      }
    : {};

  return <div {...props}>{children}</div>;
}

export function DraggableWidget({
  widget,
  editMode,
  children,
}: {
  editMode: boolean;
  widget: NoCodeWidgetOut;
  children: React.ReactNode;
}) {
  const toast = useCustomToast();
  const queryClient = useQueryClient();

  const widgetMutation = useMutation({
    mutationFn: (widgetUpdate: NoCodeWidgetOut) =>
      NoCodeService.updateWidget({
        widgetId: widgetUpdate.id,
        requestBody: widgetUpdate,
      }),

    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["accounts-no-code"] });
      toast("Updated Widget", "Changes have been saved", "success");
    },
    onError: () => {},
  });

  function updateWidgetPosition(
    widget: NoCodeWidgetOut,
    rowDelta: number,
    colDelta: number,
  ) {
    if (!widget) return;

    const newRow = Math.max(widget.row + rowDelta, 1);
    const newCol = Math.min(Math.max(widget.col + colDelta, 1), 12);

    queryClient.setQueryData(
      ["accounts-no-code"],
      (oldData: NoCodeCanvasOut) => {
        if (!oldData) return oldData;

        return {
          ...oldData,
          widgets: oldData.widgets.map((w: NoCodeWidgetOut) =>
            w.id === widget.id ? { ...w, row: newRow, col: newCol } : w,
          ),
        };
      },
    );

    widgetMutation.mutate({
      ...widget,
      row: newRow,
      col: newCol,
    });
  }

  const callable = (movedRows: number, movedCols: number) =>
    updateWidgetPosition(widget, movedRows, movedCols);

  const { attributes, listeners, setNodeRef, transform } = useDraggable({
    id: widget.id,
    data: { callable },
    disabled: !editMode,
  });

  const style = {
    transform: transform
      ? `translate3d(${transform.x}px, ${transform.y}px, 0)`
      : undefined,
    cursor: "move",
  };
  const props = editMode
    ? {
        ref: setNodeRef,
        style: style,
        ...listeners,
        ...attributes,
      }
    : {};

  return <div {...props}>{children}</div>;
}

export function NoCodeDragContext({
  setIsDragging,
  children,
}: {
  setIsDragging: React.Dispatch<React.SetStateAction<boolean>>;
  children: ReactNode;
}) {
  return (
    <DndContext
      onDragStart={() => {
        setIsDragging(true);
      }}
      onDragEnd={(event: DragEndEvent) => {
        const { delta, active } = event;
        const deltaX = delta.x;
        const deltaY = delta.y;
        const movedCols = Math.round(deltaX / 100);
        const movedRows = Math.round(deltaY / 40);

        const data = active.data;

        data.current!.callable(movedRows, movedCols);
        setIsDragging(false);
      }}
    >
      {children}
    </DndContext>
  );
}
