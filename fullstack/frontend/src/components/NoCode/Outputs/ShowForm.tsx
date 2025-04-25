import { useDisclosure, Dialog,Button, CloseButton } from "@chakra-ui/react"
import { ShowProps } from "./ShowTypes"
import { renderNoCodeParameter } from "./Show"

export function ShowForm({ widget, updateAParameter }: ShowProps) {

    const statusDisclosure = useDisclosure()


    return (
    <>
    <Button onClick={statusDisclosure.onOpen}>
        {widget.name}
        </Button>
    <Dialog.Root
      open={statusDisclosure.open}
      onExitComplete={statusDisclosure.onClose}
      onInteractOutside={statusDisclosure.onClose}
      motionPreset={'slide-in-bottom'}
    >
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content>
            <Dialog.Header>
              <Dialog.Title>{widget.name}</Dialog.Title>
              <Dialog.CloseTrigger onClick={statusDisclosure.onClose} asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
            {widget.parameters.map((param)=>(
                renderNoCodeParameter(param,(parameter) =>updateAParameter(parameter, false), statusDisclosure.onClose)
               ))}
          </Dialog.Content>
        </Dialog.Positioner>
    </Dialog.Root>
</>
)
}

