import {
  Button,

  FieldErrorText,
  FieldLabel,
  FieldRoot,
  Input,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { type SubmitHandler, useForm } from "react-hook-form"

import {  DialogBackdrop,
    DialogBody,
    DialogCloseTrigger,
    DialogContent,
    DialogFooter,
    DialogHeader,
    DialogRoot,
    DialogTitle,} from "@/components/ui/dialog"
import {
  type ApiError,
  TransactionEdit,
  TransactionOut,
  TransactionsService,
} from "../../client"
import useCustomToast from "../../hooks/useCustomToast"
import { emailPattern, handleError } from "../../utils"

interface EditTransactionProps {
  transaction: TransactionOut
  isOpen: boolean
  onClose: () => void
}


const EditTransaction = ({ transaction, isOpen, onClose }: EditTransactionProps) => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()

  const {
    register,
    handleSubmit,
    reset,
    getValues,
    formState: { errors, isSubmitting, isDirty },
  } = useForm<TransactionEdit>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: transaction,
  })

  const mutation = useMutation({
    mutationFn: (data: TransactionEdit) =>
      TransactionsService.updateTransaction({
        requestBody: data,
      }),
    onSuccess: () => {
      showToast("Success!", "Transaction updated successfully.", "success")
      onClose()
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
    onSettled: () => {
      queryClient.invalidateQueries({ queryKey: ["transactions"] })
    },
  })

  const onSubmit: SubmitHandler<TransactionEdit> = async (data) => {

    mutation.mutate(data)
  }

  const onCancel = () => {
    reset()
    onClose()
  }


  console.log("opening up")

  return (
    <DialogRoot open={isOpen} onOpenChange={onClose} modal>
      <DialogBackdrop />
      <DialogContent style={{ backgroundColor: "background" }} onSubmit={handleSubmit(onSubmit)} as='form'>
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

          <FieldRoot mt={4} invalid={!!errors.date_of_transaction}>
            <FieldLabel htmlFor="date_of_transaction">Date of Transaction</FieldLabel>
            <Input
              id="date_of_transaction"
              {...register("date_of_transaction", {
                required: "Date is required",
                valueAsDate: true,
              })}
              placeholder="Date"
              type="date"
            />
            {errors.date_of_transaction && (
              <FieldErrorText>{errors.date_of_transaction.message}</FieldErrorText>
            )}
          </FieldRoot>

          <FieldRoot mt={4} invalid={!!errors.category_id}>
            <FieldLabel htmlFor="category_id">Category</FieldLabel>
            <Input
              id="category_id"
              {...register("category_id", {
                required: "Category is required",
              })}
              placeholder="Category"
              type="number"
            />
            {errors.category_id && (
              <FieldErrorText>{errors.category_id.message}</FieldErrorText>
            )}
          </FieldRoot>
        </DialogBody>

        <DialogFooter gap={3}>
          <Button
            variant="outline"
            type="submit"
            loading={isSubmitting}
            disabled={!isDirty}
          >
            Save
          </Button>
          <DialogCloseTrigger asChild>
            <Button onClick={onCancel}>Cancel</Button>
          </DialogCloseTrigger>
        </DialogFooter>
      </DialogContent>
    </DialogRoot>
  )
}
export default EditTransaction
