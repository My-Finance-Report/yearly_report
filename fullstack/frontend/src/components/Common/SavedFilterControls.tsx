import type { SavedFilterOut } from "@/client";
import {
  DialogBody,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogRoot,
} from "@/components/ui/dialog";
import { useFilters } from "@/contexts/FilterContext";
import {
  Box,
  Button,
  FieldLabel,
  Dialog,
  FieldRoot,
  Flex,
  Input,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  Textarea,
  Portal,
  createListCollection,
  CloseButton,
  Span,
} from "@chakra-ui/react";
import React, { useRef, useState } from "react";
import { FiSave, FiTrash } from "react-icons/fi";

interface FilterSelectItem {
  label: string;
  value: string;
  description: string | null | undefined;
}

function formatFiltersForSelect(filters: SavedFilterOut[]): {
  items: FilterSelectItem[];
} {
  return {
    items: filters.map((filter) => ({
      label: filter.name,
      value: filter.id.toString(),
      description: filter.description,
    })),
  };
}

export function SavedFilterControls() {
  const { savedFilters, setCurrentFilter, currentFilter } = useFilters();

  const [isSaveDialogOpen, setIsSaveDialogOpen] = useState(false);
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false);

  const handleLoadFilter = (selectedFilterId: string) => {
    if (!selectedFilterId) return;

    const filter = [...savedFilters].find((f) => f.id.toString() === selectedFilterId);
    if (!filter) return;

    setCurrentFilter(filter);
  };

  const handleDeleteDialogOpenChange = () => {
    setIsDeleteDialogOpen(!isDeleteDialogOpen);
  };

  const handleSaveDialogOpenChange = () => {
    setIsSaveDialogOpen(!isSaveDialogOpen);
  };

  const myFilters = formatFiltersForSelect(savedFilters);

  return (
    <>
      <Flex direction="row" gap={2} position={"absolute"} top={3} right={5}>
        <Button
          size="sm"
          variant="outline"
          onClick={() => setIsSaveDialogOpen(true)}
        >
          <FiSave />
        </Button>
        <Button
          size="sm"
          disabled={!currentFilter?.is_deleteable}
          variant="outline"
          onClick={() => setIsDeleteDialogOpen(true)}
        >
          <FiTrash />
        </Button>
      </Flex>

      <FieldRoot mt={12}>
        <SelectRoot
          value={[currentFilter?.id?.toString() || myFilters.items[0]?.value]}
          collection={createListCollection({
            items: [...myFilters.items],
          })}
          onValueChange={(value) => {
            handleLoadFilter(value.value[0]);
          }}
        >
          <SelectTrigger>
            <SelectValueText placeholder="Select a filter" />
          </SelectTrigger>
          <SelectContent>
            {myFilters.items.map((item) => (
              <SelectItem key={item.value} item={item}>
                <Box>
                  <Box fontWeight="medium">{item.label}</Box>
                  {item.description && (
                    <Box fontSize="sm" color="gray.500">
                      {item.description}
                    </Box>
                  )}
                </Box>
              </SelectItem>
            ))}
          </SelectContent>
        </SelectRoot>
      </FieldRoot>

      <SaveDialog
        isSaveDialogOpen={isSaveDialogOpen}
        setIsSaveDialogOpen={setIsSaveDialogOpen}
        handleSaveDialogOpenChange={handleSaveDialogOpenChange}
      />
      <DeleteDialog
        isDeleteDialogOpen={isDeleteDialogOpen}
        setIsDeleteDialogOpen={setIsDeleteDialogOpen}
        handleDeleteDialogOpenChange={handleDeleteDialogOpenChange}
      />
    </>
  );
}

function SaveDialog({
  isSaveDialogOpen,
  setIsSaveDialogOpen,
  handleSaveDialogOpenChange,
}: {
  isSaveDialogOpen: boolean;
  setIsSaveDialogOpen: React.Dispatch<React.SetStateAction<boolean>>;
  handleSaveDialogOpenChange: () => void;
}) {
  const [filterName, setFilterName] = useState("");
  const [filterDescription, setFilterDescription] = useState("");

  const { saveCurrentFilter } = useFilters();

  const cancelRef = useRef<HTMLButtonElement | null>(null);

  const handleSaveFilter = () => {
    if (!filterName.trim()) return;

    const newFilter = {
      name: filterName,
      description: filterDescription || null,
    };
    saveCurrentFilter(newFilter);
    setIsSaveDialogOpen(false);
  };

  return (
    <DialogRoot
      open={isSaveDialogOpen}
      onOpenChange={handleSaveDialogOpenChange}
    >
      <DialogContent>
        <DialogHeader>Save Filter</DialogHeader>
        <DialogBody>
          <FieldRoot mb={4}>
            <FieldLabel>Filter Name</FieldLabel>
            <Input
              value={filterName}
              onChange={(e) => setFilterName(e.target.value)}
              placeholder="Enter a name for your filter"
            />
          </FieldRoot>

          <FieldRoot>
            <FieldLabel>Description (optional)</FieldLabel>
            <Textarea
              value={filterDescription || ""}
              onChange={(e) => setFilterDescription(e.target.value)}
              placeholder="Enter a description for your filter"
            />
          </FieldRoot>
        </DialogBody>
        <DialogFooter>
          <Button ref={cancelRef} onClick={() => setIsSaveDialogOpen(false)}>
            Cancel
          </Button>
          <Button colorScheme="blue" onClick={handleSaveFilter} ml={3}>
            Save
          </Button>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  );
}

function DeleteDialog({
  isDeleteDialogOpen,
  setIsDeleteDialogOpen,
  handleDeleteDialogOpenChange,
}: {
  handleDeleteDialogOpenChange: () => void;
  isDeleteDialogOpen: boolean;
  setIsDeleteDialogOpen: React.Dispatch<React.SetStateAction<boolean>>;
}) {
  const { deleteFilter, currentFilter } = useFilters();

  const handleDelete = () => {
    if (!currentFilter?.is_deleteable) return;
    deleteFilter(Number(currentFilter.id));
    setIsDeleteDialogOpen(false);
  };

  return (
    <DialogRoot
      open={isDeleteDialogOpen}
      onOpenChange={handleDeleteDialogOpenChange}
    >
      <Portal>
        <Dialog.Backdrop />
        <Dialog.Positioner>
          <Dialog.Content>
            <Dialog.Header>
              <Dialog.CloseTrigger
                asChild
                position="absolute"
                top={3}
                right={3}
              >
                <CloseButton size="sm" />
              </Dialog.CloseTrigger>
              <Dialog.Title>Confirm Delete</Dialog.Title>
            </Dialog.Header>
            <Dialog.Body>
              <p>
                Are you sure you want to delete{" "}
                <Span fontWeight="bold">{currentFilter?.name}</Span>?
              </p>
            </Dialog.Body>
            <Dialog.Footer>
              <Dialog.ActionTrigger asChild>
                <Button variant="outline">Cancel</Button>
              </Dialog.ActionTrigger>
              <Button onClick={handleDelete} colorPalette="red">
                Delete
              </Button>
            </Dialog.Footer>
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </DialogRoot>
  );
}
