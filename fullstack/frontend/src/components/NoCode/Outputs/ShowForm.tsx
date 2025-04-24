import { Box,useDisclosure, Dialog, Flex ,TableRoot,Button, Text,TableHeader, TableRow, TableCell, TableBody, Heading, CloseButton, Portal } from "@chakra-ui/react"
import { NoCodeWidgetOut } from "@/client"
import { NoCodeParameter } from "../Generators/Parameter"

export function ShowForm({ widget }: { widget: NoCodeWidgetOut }) {

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
      <Portal>
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
                <NoCodeParameter parameter={param} onChange={()=>{}} />
               ))}
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </Dialog.Root>
</>
)
}

