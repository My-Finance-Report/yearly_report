import {
  Box,
  Switch,
  Field,
  Flex,
  Heading,
  Text,
} from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { useState } from "react"

import { type ApiError, UsersService } from "../../client"
import useAuth from "../../hooks/useAuth"
import useCustomToast from "../../hooks/useCustomToast"
import { handleError } from "../../utils"
import { HiCheck, HiX } from "react-icons/hi"

const PowerUserToggle = () => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()
  const { user: currentUser } = useAuth()
  const [isPowerUser, setIsPowerUser] = useState(
    currentUser?.settings?.power_user_filters || false
  )

  const mutation = useMutation({
    mutationFn: (isPowerUser: boolean) =>
      UsersService.updateUserMe({
        requestBody: {
          email: currentUser?.email || "",
          settings: {
            has_budget: currentUser?.settings?.has_budget || false,
            power_user_filters: isPowerUser,
          },
        },
      }),
    onSuccess: () => {
      showToast(
        "Success!",
        "Power user filter settings updated successfully.",
        "success"
      )
      queryClient.invalidateQueries({ queryKey: ["currentUser"] })
    },
    onError: (err: ApiError) => {
      handleError(err, showToast)
      // Reset to previous state on error
      setIsPowerUser(currentUser?.settings?.power_user_filters || false)
    },
  })

  const handleToggle = () => {
    const newValue = !isPowerUser
    setIsPowerUser(newValue)
    mutation.mutate(newValue)
  }

  return (
    <Box mt={6}>
      <Heading size="sm" mb={2}>
        Advanced Features
      </Heading>
      <Field.Root>
        <Flex alignItems="center" justifyContent="space-between">
          <Box>
            <Text fontWeight="medium">Power User Filters</Text>
            <Text fontSize="sm" >
              Enable advanced filtering options for transactions and reports
            </Text>
          </Box>
         <Switch.Root variant='solid'  size="lg" disabled={mutation.isPending} checked={isPowerUser} onCheckedChange={handleToggle}> 
  <Switch.HiddenInput />
      <Switch.Control>
        <Switch.Thumb>
          <Switch.ThumbIndicator fallback={<HiX color="black" />}>
            <HiCheck />
          </Switch.ThumbIndicator>
        </Switch.Thumb>
      </Switch.Control>
      <Switch.Label></Switch.Label>
          </Switch.Root>
        </Flex>
      </Field.Root>
    </Box>
  )
}

export default PowerUserToggle
