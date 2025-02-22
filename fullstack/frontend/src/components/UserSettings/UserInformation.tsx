"use client"

import {
  Box,
  Button,
  Container,
  Field,
  Flex,
  Heading,
  Input,
  Text,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { useState } from "react"
import { type SubmitHandler, useForm } from "react-hook-form"

import { useColorModeValue } from "@/components/ui/color-mode"
import {
  type ApiError,
  type UserOut,
  type UserUpdateMe,
  UsersService,
} from "../../client"
import useAuth from "../../hooks/useAuth"
import useCustomToast from "../../hooks/useCustomToast"
import { emailPattern, handleError } from "../../utils"

const UserInformation = () => {
  const queryClient = useQueryClient()
  const color = useColorModeValue("inherit", "ui.light")
  const showToast = useCustomToast()
  const [editMode, setEditMode] = useState(false)
  const { user: currentUser } = useAuth()
  const {
    register,
    handleSubmit,
    reset,
    getValues,
    formState: { isSubmitting, errors, isDirty },
  } = useForm<UserOut>({
    mode: "onBlur",
    criteriaMode: "all",
    defaultValues: {
      full_name: currentUser?.full_name,
      email: currentUser?.email,
    },
  })

  const toggleEditMode = () => {
    setEditMode(!editMode)
  }

  const mutation = useMutation({
    mutationFn: (data: UserUpdateMe) =>
      UsersService.updateUserMe({ requestBody: data }),
    onSuccess: () => {
      showToast("Success!", "User updated successfully.", "success")
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
    },
    onSettled: () => {
      queryClient.invalidateQueries()
    },
  })

  const onSubmit: SubmitHandler<UserUpdateMe> = async (data) => {
    mutation.mutate(data)
  }

  const onCancel = () => {
    reset()
    toggleEditMode()
  }

  return (
    <Container maxW="full">
      <Heading size="sm" py={4}>
        User Information
      </Heading>
      <Box
        w={{ sm: "full", md: "50%" }}
        as="form"
        onSubmit={handleSubmit(onSubmit)}
      >
        <Field.Root>
          <Field.Label color={color} htmlFor="name">
            Full Name
          </Field.Label>
          {editMode ? (
            <Input
              id="name"
              {...register("full_name", { maxLength: 30 })}
              type="text"
              size="md"
              w="auto"
            />
          ) : (
            <Text
              py={2}
              color={!currentUser?.full_name ? "ui.dim" : "inherit"}
              truncate
              maxWidth="250px"
            >
              {currentUser?.full_name || "N/A"}
            </Text>
          )}
        </Field.Root>

        <Field.Root mt={4} invalid={!!errors.email}>
          <Field.Label color={color} htmlFor="email">
            Email
          </Field.Label>
          {editMode ? (
            <Input
              id="email"
              {...register("email", {
                required: "Email is required",
                pattern: emailPattern,
              })}
              type="email"
              size="md"
              w="auto"
            />
          ) : (
            <Text py={2} truncate maxWidth="250px">
              {currentUser?.email}
            </Text>
          )}
          {errors.email && (
            <Field.ErrorText>{errors.email.message}</Field.ErrorText>
          )}
        </Field.Root>

        <Flex mt={4} gap={3}>
          <Button
            variant="outline"
            onClick={toggleEditMode}
            type={editMode ? "button" : "submit"}
            loading={editMode ? isSubmitting : false}
            disabled={editMode ? !isDirty || !getValues("email") : false}
          >
            {editMode ? "Save" : "Edit"}
          </Button>
          {editMode && (
            <Button onClick={onCancel} disabled={isSubmitting}>
              Cancel
            </Button>
          )}
        </Flex>
      </Box>
    </Container>
  )
}

export default UserInformation
