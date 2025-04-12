import { WorkerStatusService, WorkerStatusOut } from "@/client";
import {  format } from 'date-fns';
import {
  Badge,
  Text,
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
  Flex,
  CloseButton,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { LuCheck, LuX } from "react-icons/lu";

function determineColor(workerStatus: WorkerStatusOut, isLast: boolean) {
  let color =
    workerStatus.status == "failed" ? "red" : isLast ? "yellow" : "green";

  if (isLast && workerStatus.status == "completed") {
    color = "green";
  }
  return color;
}

function determineIcon(workerStatus: WorkerStatusOut, isLast: boolean, inTimeline: boolean){
    const Wrapper = inTimeline ? TimelineIndicator : Box

  let displayStatusIcon =
    workerStatus.status == "failed" ? (
      <Wrapper>
        <LuX />
      </Wrapper>
    ) : isLast ? (
      <Spinner size="sm" />
    ) : (
      <Wrapper>
        <LuCheck />
      </Wrapper>
    );
  if (isLast && workerStatus.status == "completed") {
    displayStatusIcon = (
      <Wrapper>
        <LuCheck />
      </Wrapper>
    );
  }

  return displayStatusIcon
}


function TimelineEntry({
  workerStatus,
  isLast,
  includeDesc = true,
}: {
  workerStatus: WorkerStatusOut;
  isLast: boolean;
  includeDesc?: boolean;
}) {


  const color = determineColor(workerStatus,isLast)
  const DisplayStatusIcon = determineIcon(workerStatus,isLast, true)

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
  );
}

export function WorkerStatus() {
  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
  });

  if (isLoading) {
    return null
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
    return null;
  }

  if (!data || data.length === 0) {
    return null
  }

  const latestStatus = data[data.length - 1];
  const lastSyncDate = new Date(`${latestStatus.created_at}`);
  const formattedLocalTime = format(lastSyncDate, 'MMM d, yyyy h:mm a');

  return (
    <Dialog.Root
      onExitComplete={statusDisclosure.onClose}
 motionPreset="slide-in-bottom"
    >
      <DialogTrigger>
        <Flex alignItems="center" gap={2} cursor="pointer" p={2}>
          <Badge p={2} colorPalette={determineColor(latestStatus, true)}>
          {determineIcon(latestStatus, true, false)}
            {latestStatus.status}</Badge>
        </Flex>
      </DialogTrigger>
      <Portal>
        <DialogBackdrop />
        <DialogPositioner>
          <DialogContent>
            <Dialog.Header>
                <Dialog.Title>Transaction Upload Status</Dialog.Title>
                <Text>{formattedLocalTime}</Text>
              <Dialog.CloseTrigger asChild>
                <CloseButton position="absolute" right={4} top={4} size="sm" />
              </Dialog.CloseTrigger>
            </Dialog.Header>
           
            <Box p={4} pl={10} flex={"column"}>
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
