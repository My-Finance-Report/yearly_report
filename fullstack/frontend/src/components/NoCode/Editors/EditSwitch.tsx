import { HStack, Text, Switch, Heading } from "@chakra-ui/react";
import { HiCheck, HiX } from "react-icons/hi";

export function EditSwitch({
  editMode,
  setEditMode,
}: {
  editMode: boolean;
  setEditMode: React.Dispatch<React.SetStateAction<boolean>>;
}) {
  return (
    <>
      <HStack>
        <Text>Enable Edit Mode</Text>
        <Switch.Root
          variant="solid"
          size="lg"
          checked={editMode}
          onCheckedChange={() => setEditMode((prev) => !prev)}
        >
          <Switch.HiddenInput />
          <Switch.Control>
            <Switch.Thumb>
              <Switch.ThumbIndicator fallback={<HiX color="black" />}>
                <HiCheck />
              </Switch.ThumbIndicator>
            </Switch.Thumb>
          </Switch.Control>
          <Switch.Label />
        </Switch.Root>
      </HStack>
      {editMode && (
        <Heading>Click on edit button to edit, and drag to rearrange</Heading>
      )}
    </>
  );
}
