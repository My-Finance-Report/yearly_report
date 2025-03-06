import {
  Button,
  Text,
  FieldErrorText,
  FieldLabel,
  FieldRoot,
  Tag,
  Input,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectValueText,
  SelectRoot,
  createListCollection,
  HStack,
  VStack,
  CloseButton,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { type SubmitHandler, useForm, Controller } from "react-hook-form";

import {
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogRoot,
  DialogTitle,
} from "@/components/ui/dialog";
import {
  type ApiError,
  BudgetEntryOut,
  TransactionsService,
  CategoryOut,
  BudgetsService,
} from "../../client";
import useCustomToast from "../../hooks/useCustomToast";
import { handleError } from "../../utils";
import { Blah } from "./SankeyConfig"; //TODO stop importing this

interface EditBudgetEntryProps {
  budgetEntry: BudgetEntryOut;
  isOpen: boolean;
  onClose: () => void;
}

function rawCategoiesToSelectItems(categories: CategoryOut[]): {
  items: Blah[];
} {
  const blahs = categories.map((category) => ({
    label: category.stylized_name,
    value: category.id,
  }));
  return { items: blahs };
}

type FormInput = {
    amount: number;
    name: string;
    budget_id: number;
    id: number;
    category_links: Array<number>;
};



export default function EditBudgetEntry({
  budgetEntry,
  isOpen,
  onClose,
}: EditBudgetEntryProps) {
  const queryClient = useQueryClient();
  const showToast = useCustomToast();

  console.log(budgetEntry);

  const {
    register,
    handleSubmit,
    reset,
    control,
    formState: { errors, isSubmitting, isDirty },
  } = useForm<FormInput>({
    mode: "onBlur",
    criteriaMode: "all",
  });


  const { data, isLoading } = useQuery({
    queryKey: ["categories"],
    queryFn: () => TransactionsService.listAllCategories(),
  });

  const categories = rawCategoiesToSelectItems(data ?? []);

  const mutation = useMutation({
    mutationFn: (data: FormInput) => {
      return BudgetsService.updateBudgetEntry({
        entryId: budgetEntry.id,
        requestBody: {
          ...data,
          budget_id: budgetEntry.budget_id,
          id: budgetEntry.id,
          amount: data.amount,
          category_links: data.category_links.map((link) => ({category_id: link, entry_id: budgetEntry.id})),
        },
      });
    },
    onSuccess: () => {
      showToast("Success!", "Transaction updated successfully.", "success");
      onClose();
    },
    onError: (err: ApiError) => {
      handleError(err, showToast);
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["budgets"] });
    },
  });

  const onSubmit: SubmitHandler<FormInput> = async (data) => {
    mutation.mutate(data);
  };

  const onCancel = () => {
    reset();
    onClose();
  };

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose} modal>
      <DialogBackdrop />
      <DialogContent
        style={{ backgroundColor: "background" }}
        onSubmit={handleSubmit(onSubmit)}
        as="form"
      >
        <DialogHeader>
          <DialogTitle>Edit Budget Entry</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <FieldRoot invalid={!!errors.name} required>
            <FieldLabel htmlFor="name">Name</FieldLabel>
            <Input
              id="name"
              {...register("name", {
                required: "Name is required",
              })}
              placeholder="Name"
              type="text"
              defaultValue={budgetEntry?.name}
            />
            {errors.name && (
              <FieldErrorText>{errors.name.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.amount}>
            <FieldLabel htmlFor="amount">Amount</FieldLabel>
            <Input
              id="amount"
              {...register("amount", {
                required: "Amount is required",
              })}
              placeholder="Amount"
              type="number"
              defaultValue={budgetEntry?.amount}
            />
            {errors.amount && (
              <FieldErrorText>{errors.amount.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.category_links}>
            <FieldLabel htmlFor="category_links">Category Links</FieldLabel>
            <Controller
              control={control}
              name="category_links"
              render={({ field }) => {
                const { onChange, value } = field;
                console.log(value);
                return (
                    <>
                        <VStack>
                        {value?.map((category_id: number) => (
                            <CategoryLink onRemove={()=>onChange(value.filter((c) => c !== category_id))} key={category_id} category={data?.find((c) => c.id === category_id)!} />
                        ))}
                        </VStack>
                  <SelectRoot
                    id="category_links"
                    placeholder="Select a category"
                    multiple
                    size="sm"
                    collection={createListCollection(categories)}
                    onValueChange={(val) => {
                      onChange(val.value);
                    }}
                  >
                    <SelectTrigger>
                      <SelectValueText placeholder="Select a category" />
                    </SelectTrigger>
                    <SelectContent>
                      {categories.items.map((cat) => (
                        <SelectItem key={cat.value} item={cat}>
                          {cat.label}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </SelectRoot>
</>
                );
              }}
            />
            {errors.category_links && (
              <FieldErrorText>{errors.category_links.message}</FieldErrorText>
            )}
          </FieldRoot>
        </DialogBody>

        <DialogFooter gap={3}>
          <HStack>
            <Button variant="outline" onClick={onCancel} title="Cancel">
              Cancel
            </Button>
            <Button type="submit" loading={isSubmitting}>
              Save
            </Button>
          </HStack>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  );
}


function CategoryLink({category, onRemove}: {category: CategoryOut, onRemove: (category: CategoryOut) => void}) {
   return ( 
    <Tag.Root size="sm" >
    <Text key={category.id}>{category.stylized_name}</Text>
    <CloseButton onClick={() => onRemove(category)} />
    </Tag.Root>
)
}