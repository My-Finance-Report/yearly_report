import { WorkerStatusService, WorkerStatusOut } from "@/client";
import { CloseIcon } from "@chakra-ui/icons";
import {
  Badge,
  Box,
  Timeline,
  Spinner,
  TimelineIndicator,
  useDisclosure,
  Dialog,
  DialogPositioner,
  DialogContent,
  DialogFooter,
  Button,
  DialogTrigger,
  Portal,
  DialogBackdrop,
  DialogCloseTrigger,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { LuCheck, LuX } from "react-icons/lu";

function TimelineEntry({
  workerStatus,
  isLast,
  includeDesc = true,
}: {
  workerStatus: WorkerStatusOut;
  isLast: boolean;
  includeDesc?: boolean;
}) {
  let color =
    workerStatus.status == "failed" ? "red" : isLast ? "yellow" : "green";

  let displayStatusIcon =
    workerStatus.status == "failed" ? (
      <TimelineIndicator>
        <LuX />
      </TimelineIndicator>
    ) : isLast ? (
      <Spinner size="sm" />
    ) : (
      <TimelineIndicator>
        <LuCheck />
      </TimelineIndicator>
    );
  if (isLast && workerStatus.status == "completed") {
    displayStatusIcon = (
      <TimelineIndicator>
        <LuCheck />
      </TimelineIndicator>
    );
    color = "green";
  }

  return (
    <Timeline.Item>
      <Timeline.Connector>
        <Timeline.Separator />
        {displayStatusIcon}
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
  );
}

export function WorkerStatus() {
  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
  });

  if (isLoading) {
    return <Box flex={1}>Loading...</Box>;
  }

  return (
    <Box flex={1}>
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
  );
}

export function CollapsibleWorkerStatus() {
  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
  });
  const statusDisclosure = useDisclosure();

  if (isLoading) {
    return <Box flex={1}>Loading...</Box>;
  }

  if (!data || data.length === 0) {
    return <Box flex={1}>No status available</Box>;
  }

  const latestStatus = data[data.length - 1];

  return (
    <Dialog.Root
      onExitComplete={statusDisclosure.onClose}
      size={{ base: "sm", md: "md" }}
    >
      <DialogTrigger>
        <Box cursor="pointer" p={2}>
          <Timeline.Root size="sm">
            <TimelineEntry
              workerStatus={latestStatus}
              isLast={true}
            />
          </Timeline.Root>
        </Box>
      </DialogTrigger>
      <Portal>
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <DialogCloseTrigger position="absolute" right={4} top={4}>
              <CloseIcon />
            </DialogCloseTrigger>
            <Box p={4}>
              <Timeline.Root size="sm" mt={2}>
                {data.map((status, index) => (
                  <TimelineEntry
                    key={status.id}
                    workerStatus={status}
                    isLast={index === data.length - 1}
                  />
                ))}
              </Timeline.Root>
            </Box>
            <DialogFooter gap={3}>
              <DialogCloseTrigger>
                <Button>Close</Button>
              </DialogCloseTrigger>
            </DialogFooter>
          </DialogContent>
        </DialogPositioner>
      </Portal>
    </Dialog.Root>
  );
}
