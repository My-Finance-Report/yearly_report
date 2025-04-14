import { Box, Heading, Switch, Text } from "@chakra-ui/react"
import { useMutation, useQueryClient } from "@tanstack/react-query"
import { useState } from "react"

import { useColorMode } from "@/components/ui/color-mode"
import { HiCheck, HiMoon, HiSun, HiX } from "react-icons/hi"
import { type ApiError, UsersService } from "../../client"
import useAuth from "../../hooks/useAuth"
import useCustomToast from "../../hooks/useCustomToast"
import { handleError } from "../../utils"

const AdvancedFeatures = () => {
  const queryClient = useQueryClient()
  const showToast = useCustomToast()
  const { user: currentUser } = useAuth()
  const [isPowerUser, setIsPowerUser] = useState(
    currentUser?.settings?.power_user_filters || false,
  )
  const { colorMode, toggleColorMode } = useColorMode()
  const isDarkMode = colorMode === "dark"

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
        "success",
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
      <Heading size="md" mb={4}>
        Advanced Features
      </Heading>

      <Box
        display="grid"
        gridTemplateColumns="1fr auto"
        columnGap={4}
        rowGap={4}
      >
        <Box>
          <Text fontWeight="medium">Power User Filters</Text>
          <Text fontSize="sm">
            Enable advanced filtering options for transactions and reports
          </Text>
        </Box>
        <Box display="flex" alignItems="center" justifyContent="center">
          <Switch.Root
            variant="solid"
            size="lg"
            disabled={mutation.isPending}
            checked={isPowerUser}
            onCheckedChange={handleToggle}
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
        </Box>

        <Box>
          <Text fontWeight="medium">Dark Mode</Text>
          <Text fontSize="sm">Switch between light and dark theme</Text>
        </Box>
        <Box display="flex" alignItems="center" justifyContent="center">
          <Switch.Root
            variant="solid"
            size="lg"
            checked={isDarkMode}
            onCheckedChange={toggleColorMode}
          >
            <Switch.HiddenInput />
            <Switch.Control>
              <Switch.Thumb>
                <Switch.ThumbIndicator fallback={<HiSun color="black" />}>
                  <HiMoon />
                </Switch.ThumbIndicator>
              </Switch.Thumb>
            </Switch.Control>
            <Switch.Label />
          </Switch.Root>
        </Box>
      </Box>
    </Box>
  )
}

export default AdvancedFeatures
