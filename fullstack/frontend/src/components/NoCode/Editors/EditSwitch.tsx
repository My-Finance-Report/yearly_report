import { HStack, Switch } from "@chakra-ui/react";
import { HiPencil, HiX } from "react-icons/hi";

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
        <Switch.Root
          variant="solid"
          size="lg"
          checked={editMode}
          onCheckedChange={() => setEditMode((prev) => !prev)}
        >
          <Switch.HiddenInput />
          <Switch.Control>
            <Switch.Thumb>
              <Switch.ThumbIndicator fallback={<HiPencil color="black" />}>
                <HiX />
              </Switch.ThumbIndicator>
            </Switch.Thumb>
          </Switch.Control>
          <Switch.Label />
        </Switch.Root>
      </HStack>
    </>
  );
}
