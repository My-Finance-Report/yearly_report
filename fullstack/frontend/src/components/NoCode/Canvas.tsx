import {
  NoCodeWidget,
  renderNoCodeParameter,
} from "@/components/NoCode/Outputs/Show";
import { Route } from "@/routes/_layout/_logged_in/accounts";
import { EditModal } from "@/components/NoCode/Outputs/EditWidget";
import { EditParameterModal } from "@/components/NoCode/Outputs/EditParameter";
import { EditSwitch } from "@/components/NoCode/Editors/EditSwitch";
import { WidgetBuilder } from "@/components/NoCode/Editors/WidgetBuilder/Builder";
import {
  DraggableWidget,
  DraggableParameter,
  NoCodeDragContext,
} from "@/components/NoCode/Editors/Dragables";
import { useNoCodeContext } from "@/contexts/NoCodeContext";
import { NoCodeWidgetOut, Parameter_Output } from "@/client";
import {
  Grid,
  GridItem,
  Container,
  HStack,
  Button,
  useDisclosure,
} from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { useNavigate } from "@tanstack/react-router";

function DummyGridBacking({
  isEditMode,
  isDragging,
}: {
  isEditMode: boolean;
  isDragging: boolean;
}) {
  if (!isEditMode || !isDragging) {
    return null;
  }

  return Array.from({ length: 100 }, (_, i) => i + 1).map((row) =>
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map((col) => (
      <GridItem
        borderWidth={3}
        borderColor="red.300"
        opacity={isDragging ? 0.5 : 0}
        pointerEvents={isDragging ? "none" : "auto"}
        key={`background-${row}-${col}`}
        rowStart={row}
        colStart={col}
        rowSpan={1}
        colSpan={1}
      ></GridItem>
    )),
  );
}

function NoCodeDraggableAndEditableParam({
  param,
  editMode,
  canvasId,
}: {
  param: Parameter_Output;
  editMode: boolean;
  canvasId: number;
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
      <EditParameterModal param={param} editMode={editMode} canvasId={canvasId}>
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
  canvasId,
}: {
  widget: NoCodeWidgetOut;
  editMode: boolean;
  canvasId: number;
}) {
  return (
    <GridItem
      border={editMode ? "1px solid red" : "none"}
      key={widget.id || widget.name}
      rowStart={widget.row}
      colStart={widget.col}
      rowSpan={widget.row_span}
      colSpan={widget.col_span}
      overflow="auto"
    >
      <EditModal widget={widget} editMode={editMode} canvasId={canvasId}>
        <DraggableWidget widget={widget} editMode={editMode}>
          <NoCodeWidget widget={widget} />
        </DraggableWidget>
      </EditModal>
    </GridItem>
  );
}

export function NoCodeDisplayCanvas({
  widgets,
  canvasId,
}: {
  widgets: NoCodeWidgetOut[];
  canvasId: number;
}) {
  const [isDragging, setIsDragging] = useState(false);
  const widgetBuilder = useDisclosure();
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
    <>
      <Container w="100%">
        <HStack p={4} gap={4}>
          <EditSwitch
            editMode={isEditMode}
            setEditMode={handleEditModeChange}
          />
          <Button onClick={widgetBuilder.onOpen} size="sm" variant="outline">
            Add Widget
          </Button>
        </HStack>
        <NoCodeDragContext setIsDragging={setIsDragging}>
          <Grid
            templateRows={`repeat(60, 40px)`}
            templateColumns={`repeat(12, 100px)`}
            gap={4}
          >
            <DummyGridBacking isEditMode={isEditMode} isDragging={isDragging} />

            {widgets.map((widget, index) => (
              <NoCodeDraggableAndEditableWidget
                key={index}
                widget={widget}
                editMode={isEditMode}
                canvasId={canvasId}
              />
            ))}
            {paramsToDisplay.map((param, index) => (
              <NoCodeDraggableAndEditableParam
                key={index}
                param={param}
                editMode={isEditMode}
                canvasId={canvasId}
              />
            ))}
          </Grid>
        </NoCodeDragContext>
      </Container>
      <WidgetBuilder
        isOpen={widgetBuilder.open}
        onClose={widgetBuilder.onClose}
      />
    </>
  );
}
