import { type WorkerStatusOut, WorkerStatusService } from "@/client"
import useAuth from "@/hooks/useAuth"
import {
  Badge,
  Box,
  CloseButton,
  Dialog,
  Flex,
  Portal,
  Spinner,
  Text,
  Timeline,
  TimelineIndicator,
  useDisclosure,
} from "@chakra-ui/react"
import { useQuery } from "@tanstack/react-query"
import { Link, useRouterState } from "@tanstack/react-router"
import { format } from "date-fns"
import { useEffect, useState } from "react"
import { LuCheck, LuX } from "react-icons/lu"

function determineColor(workerStatus: WorkerStatusOut, isLast: boolean) {
  let color =
    workerStatus.status === "failed" ? "red" : isLast ? "yellow" : "green"

  if (isLast && workerStatus.status === "completed") {
    color = "green"
  }
  return color
}

function determineIcon(
  workerStatus: WorkerStatusOut,
  isLast: boolean,
  inTimeline: boolean,
) {
  const Wrapper = inTimeline ? TimelineIndicator : Box

  let displayStatusIcon =
    workerStatus.status === "failed" ? (
      <Wrapper>
        <LuX />
      </Wrapper>
    ) : isLast ? (
      <Spinner size="sm" />
    ) : (
      <Wrapper>
        <LuCheck />
      </Wrapper>
    )
  if (isLast && workerStatus.status === "completed") {
    displayStatusIcon = (
      <Wrapper>
        <LuCheck />
      </Wrapper>
    )
  }

  return displayStatusIcon
}

function TimelineEntry({
  workerStatus,
  isLast,
  includeDesc = true,
}: {
  workerStatus: WorkerStatusOut
  isLast: boolean
  includeDesc?: boolean
}) {
  const color = determineColor(workerStatus, isLast)
  const DisplayStatusIcon = determineIcon(workerStatus, isLast, true)

  return (
    <Timeline.Item>
      <Timeline.Connector>
        <Timeline.Separator />
        {DisplayStatusIcon}
      </Timeline.Connector>
      <Timeline.Content textStyle="xs">
        <Timeline.Title>
          <Badge colorPalette={color}>{workerStatus.status}</Badge>
        </Timeline.Title>
        {includeDesc && (
          <Timeline.Description>
            {workerStatus.additional_info}
          </Timeline.Description>
        )}
      </Timeline.Content>
    </Timeline.Item>
  )
}

export function WorkerStatus() {
  const [isCompleted, setIsCompleted] = useState(false)
  const { user } = useAuth()
  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
    enabled: !!user?.id,
    refetchInterval: isCompleted ? undefined : 5000,
  })

  useEffect(() => {
    if (data?.some((status) => status.status === "completed")) {
      setIsCompleted(true)
    }
  }, [data])

  if (isLoading) {
    return null
  }

  return (
    <Flex direction="column" alignItems="center" justifyContent="center">
      <Box flex={1}>
        <Text fontSize="lg" fontWeight="bold" mb={2}>
          Transaction Upload Status
        </Text>
        <Timeline.Root size="sm">
          {data?.map((status, index) => (
            <TimelineEntry
              key={status.id}
              workerStatus={status}
              isLast={index === data.length - 1}
            />
          ))}
        </Timeline.Root>
      </Box>
    </Flex>
  )
}

export function CollapsibleWorkerStatus() {
  const { user } = useAuth()


  const { data, isLoading, refetch } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
    enabled: !!user?.id,
    refetchInterval: (data)=>(data?.state.data?.some((status) => status.status === "completed") ? undefined : 5000)
  })
  const {location }= useRouterState()

  useEffect(() => {
    refetch()
  }, [location])

  const statusDisclosure = useDisclosure()

  if (isLoading) {
    return null
  }

  if (!data || data.length === 0) {
    return null
  }

  const latestStatus = data[data.length - 1]
  const lastSyncDate = new Date(`${latestStatus.created_at}`)
  const formattedLocalTime = format(lastSyncDate, "MMM d, yyyy h:mm a")

  return (
    <>
    <Flex onClick={statusDisclosure.onOpen} alignItems="center" gap={2} cursor="pointer" p={2}>
          <Badge p={2} colorPalette={determineColor(latestStatus, true)}>
            {determineIcon(latestStatus, true, false)}
            {latestStatus.status}
          </Badge>
        </Flex>
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
              <Dialog.Title>Transaction Upload Status</Dialog.Title>
              <Dialog.CloseTrigger onClick={statusDisclosure.onClose} asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
            <Box p={4} pl={10} flex={"column"}>
              <Dialog.Description mb={4}>{formattedLocalTime}</Dialog.Description>
              <Timeline.Root size="sm" mt={2}>
                {data.map((status, index) => (
                  <TimelineEntry
                    key={status.id}
                    workerStatus={status}
                    isLast={index === data.length - 1}
                  />
                ))}
              </Timeline.Root>
              <Box  onClick={statusDisclosure.onClose} asChild>
                <Text mt={4} textDecoration="underline"><Link to="/faq" >Why is this taking so long?</Link></Text>
              </Box>
            </Box>
          </Dialog.Content>
        </Dialog.Positioner>
      </Portal>
    </Dialog.Root>
</>
  )
}
