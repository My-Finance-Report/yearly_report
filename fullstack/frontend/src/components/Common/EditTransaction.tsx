import {
  Button,
  FieldErrorText,
  FieldLabel,
  FieldRoot,
  HStack,
  Input,
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
  createListCollection,
} from "@chakra-ui/react"
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query"
import { Controller, type SubmitHandler, useForm } from "react-hook-form"

import {
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogRoot,
  DialogTitle,
} from "@/components/ui/dialog"
import {
  type ApiError,
  type CategoryOut,
  type TransactionEdit,
  type TransactionKind,
  type TransactionOut,
  TransactionsService,
} from "../../client"
import useCustomToast from "../../hooks/useCustomToast"
import { handleError } from "../../utils"
import type { Blah } from "./SankeyConfig" //TODO stop importing this

interface EditTransactionProps {
  transaction: TransactionOut
  isOpen: boolean
  onClose: () => void
}

function rawCategoiesToSelectItems(categories: CategoryOut[]): {
  items: Blah[]
} {
  const blahs = categories.map((category) => ({
    label: category.name,
    value: category.id,
  }))
  return { items: blahs }
}

const kinds: { items: { label: string; value: TransactionKind }[] } = {
  items: [
    {
      label: "Expense",
      value: "withdrawal",
    },
    {
      label: "Deposit",
      value: "deposit",
    },
  ],
}

const EditTransaction = ({
  transaction,
  isOpen,
  onClose,
}: EditTransactionProps) => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()

  const {
    register,
    handleSubmit,
    reset,
    control,
    formState: { errors, isSubmitting },
  } = useForm<TransactionEdit>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      ...transaction,
      date_of_transaction: transaction.date_of_transaction.split("T")[0],
    },
  })

  const { data } = useQuery({
    queryKey: ["categories"],
    queryFn: () =>
      TransactionsService.listCategories({ transactionId: transaction.id }),
  })

  const categories = rawCategoiesToSelectItems(data ?? [])

  const mutation = useMutation({
    mutationFn: (data: TransactionEdit) => {
      return TransactionsService.updateTransaction({
        requestBody: data,
      })
    },
    onSuccess: () => {
      showToast("Success!", "Transaction updated successfully.", "success")
      onClose()
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["aggregatedTransactions"] })
    },
  })

  const onSubmit: SubmitHandler<TransactionEdit> = async (data) => {
    mutation.mutate(data)
  }

  const onCancel = () => {
    reset()
    onClose()
  }

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose} modal>
      <DialogBackdrop />
      <DialogContent
        style={{ backgroundColor: "background" }}
        onSubmit={handleSubmit(onSubmit)}
        as="form"
      >
        <DialogHeader>
          <DialogTitle>Edit Transaction</DialogTitle>
          <DialogCloseTrigger />
        </DialogHeader>

        <DialogBody>
          <FieldRoot invalid={!!errors.description} required>
            <FieldLabel htmlFor="description">Description</FieldLabel>
            <Input
              id="description"
              {...register("description", {
                required: "Description is required",
              })}
              placeholder="Description"
              type="text"
            />
            {errors.description && (
              <FieldErrorText>{errors.description.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.date_of_transaction}>
            <FieldLabel htmlFor="date_of_transaction">
              Date of Transaction
            </FieldLabel>
            <Input
              id="date_of_transaction"
              {...register("date_of_transaction", {
                required: "Date is required",
              })}
              placeholder="Date"
              type="date"
            />
            {errors.date_of_transaction && (
              <FieldErrorText>
                {errors.date_of_transaction.message}
              </FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot invalid={!!errors.kind} required mt={4}>
            <FieldLabel htmlFor="kind">Kind</FieldLabel>
            <Controller
              control={control}
              name="kind"
              render={({ field }) => {
                const { onChange } = field
                return (
                  <SelectRoot
                    id="kind"
                    placeholder="Withdrawal / Deposit"
                    defaultValue={[
                      kinds.items.find(
                        (kind) => kind.value === transaction.kind,
                      )!.value,
                    ]}
                    collection={createListCollection(kinds)}
                    onValueChange={(val) => {
                      onChange(val.value[0])
                    }}
                  >
                    <SelectTrigger>
                      <SelectValueText placeholder="Select a kind" />
                    </SelectTrigger>
                    <SelectContent>
                      {kinds.items.map((kind) => (
                        <SelectItem key={kind.value} item={kind}>
                          {kind.label}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </SelectRoot>
                )
              }}
            />
            {errors.kind && (
              <FieldErrorText>{errors.kind.message}</FieldErrorText>
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
            />
            {errors.amount && (
              <FieldErrorText>{errors.amount.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.category_id}>
            <FieldLabel htmlFor="category_id">Category</FieldLabel>

            <Controller
              control={control}
              name="category_id"
              render={({ field }) => {
                const { onChange, value } = field
                return (
                  <SelectRoot
                    id="category_id"
                    placeholder="Select a category"
                    value={[
                      categories.items.find((cat) => cat.value === value)
                        ?.value as unknown as string,
                    ]}
                    collection={createListCollection(categories)}
                    onValueChange={(val) => {
                      onChange(val.value[0])
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
                )
              }}
            />
            {errors.category_id && (
              <FieldErrorText>{errors.category_id.message}</FieldErrorText>
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
  )
}
export default EditTransaction
