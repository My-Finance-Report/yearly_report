import {
  NoCodeShow,
  renderNoCodeParameter,
} from "@/components/NoCode/Outputs/Show";
import { Route } from "@/routes/_layout/_logged_in/accounts";
import { EditModal } from "@/components/NoCode/Outputs/EditWidget";
import { EditParameterModal } from "@/components/NoCode/Outputs/EditParameter";
import { EditSwitch } from "@/components/NoCode/Editors/EditSwitch";
import {
  DraggableWidget,
  DraggableParameter,
  NoCodeDragContext,
} from "@/components/NoCode/Editors/Dragables";
import { useNoCodeContext } from "@/contexts/NoCodeContext";
import { NoCodeWidgetIn_Output, Parameter_Output } from "@/client";
import { Grid, GridItem, Container } from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { useNavigate } from "@tanstack/react-router";

function DummyGridBacking() {
  return Array.from({ length: 100 }, (_, i) => i + 1).map((row) =>
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map((col) => (
      <GridItem
        key={`background-${row}-${col}`}
        rowStart={row}
        colStart={col}
        rowSpan={1}
        colSpan={1}
        borderWidth={3}
        borderColor="red.300"
        opacity={0.5}
        pointerEvents="none"
      />
    )),
  );
}

function NoCodeDraggableAndEditableParam({
  param,
  editMode,
}: {
  param: Parameter_Output;
  editMode: boolean;
}) {
  return (
    <GridItem
      key={`${param.name}${param.group_id.toString()}`}
      border={editMode ? "1px solid red" : "none"}
      rowStart={param.display_info!.row}
      colStart={param.display_info!.col}
      rowSpan={param.display_info!.row_span}
      colSpan={param.display_info!.col_span}
    >
      <EditParameterModal param={param} editMode={editMode}>
        <DraggableParameter param={param} editMode={editMode}>
          {renderNoCodeParameter(param)}
        </DraggableParameter>
      </EditParameterModal>
    </GridItem>
  );
}

function NoCodeDraggableAndEditableWidget({
  widget,
  editMode,
}: {
  widget: NoCodeWidgetIn_Output;
  editMode: boolean;
}) {
  return (
    <GridItem
      border={editMode ? "1px solid red" : "none"}
      key={widget.id || widget.name}
      rowStart={widget.row}
      colStart={widget.col}
      rowSpan={widget.row_span}
      colSpan={widget.col_span}
    >
      <EditModal widget={widget} editMode={editMode}>
        <DraggableWidget widget={widget} editMode={editMode}>
          <NoCodeShow widget={widget} />
        </DraggableWidget>
      </EditModal>
    </GridItem>
  );
}

export function NoCodeDisplayCanvas({
  widgets,
}: {
  widgets: NoCodeWidgetIn_Output[];
}) {
  const [isDragging, setIsDragging] = useState(false);
  const { getParamsForView, parameters } = useNoCodeContext();
  const [paramsToDisplay, setParamsToDisplay] = useState(
    getParamsForView("page"),
  );

  useEffect(() => {
    setParamsToDisplay(getParamsForView("page"));
  }, [getParamsForView, parameters]);

  const { edit } = Route.useSearch();

  const [isEditMode, setIsEditMode] = useState(edit);
  const navigation = useNavigate({ from: Route.fullPath });
  const handleEditModeChange = () => {
    const newEdit = !edit;

    navigation({
      search: () => {
        return newEdit ? { edit: true } : {};
      },
      replace: true,
    });
    setIsEditMode((prev: boolean) => !prev);
  };

  if (!widgets) {
    return <div>No widgets found</div>;
  }

  return (
    <Container w="100%">
      <EditSwitch editMode={isEditMode} setEditMode={handleEditModeChange} />
      <NoCodeDragContext setIsDragging={setIsDragging}>
        <Grid
          templateRows={`repeat(60, 40px)`}
          templateColumns={`repeat(12, 100px)`}
          gap={4}
        >
          {isDragging && DummyGridBacking()}

          {widgets.map((widget, index) => (
            <NoCodeDraggableAndEditableWidget
              key={index}
              widget={widget}
              editMode={isEditMode}
            />
          ))}
          {paramsToDisplay.map((param, index) => (
            <NoCodeDraggableAndEditableParam
              key={index}
              param={param}
              editMode={isEditMode}
            />
          ))}
        </Grid>
      </NoCodeDragContext>
    </Container>
  );
}
