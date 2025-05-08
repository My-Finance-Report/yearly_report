import { useDisclosure, Dialog, Button, CloseButton } from "@chakra-ui/react";
import { ShowProps } from "./ShowTypes";
import { renderNoCodeParameter } from "./Show";
import { useNoCodeContext } from "@/contexts/NoCodeContext";

export function ShowForm({ widget }: ShowProps) {
  const { getParamsForView } = useNoCodeContext();
  const formDisclosure = useDisclosure();

  const paramsToDisplay = getParamsForView(widget.name);

  return (
    <>
      <Button onClick={formDisclosure.onOpen}>{widget.name}</Button>
      <Dialog.Root
        open={formDisclosure.open}
        onExitComplete={formDisclosure.onClose}
        onInteractOutside={formDisclosure.onClose}
        motionPreset={"slide-in-bottom"}
      >
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content>
            <Dialog.Header>
              <Dialog.Title>{widget.name}</Dialog.Title>
              <Dialog.CloseTrigger onClick={formDisclosure.onClose} asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
            <Dialog.Body>
              {paramsToDisplay.map((param) =>
                renderNoCodeParameter(param, formDisclosure.onClose),
              )}
            </Dialog.Body>
          </Dialog.Content>
        </Dialog.Positioner>
      </Dialog.Root>
    </>
  );
}
