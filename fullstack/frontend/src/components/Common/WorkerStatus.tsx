import { WorkerStatusService, WorkerStatusOut } from "@/client";
import {Badge,Box,  Timeline, Spinner, TimelineIndicator} from '@chakra-ui/react'
import { useQuery } from "@tanstack/react-query";
import { LuCheck, LuX } from "react-icons/lu";
import { useState } from "react";

function TimelineEntry({workerStatus, isLast}: {workerStatus: WorkerStatusOut, isLast: boolean}) {
    let color = workerStatus.status == 'failed' ? 'red' : isLast ? "yellow" : "green"

    let displayStatusIcon = workerStatus.status == 'failed' ? <TimelineIndicator><LuX /></TimelineIndicator> : isLast ? <Spinner size="sm" /> : <TimelineIndicator><LuCheck /></TimelineIndicator>
    if (isLast && workerStatus.status == 'completed') {
        displayStatusIcon = <TimelineIndicator><LuCheck /></TimelineIndicator>
        color = "green"
    }


    return (
        <Timeline.Item>
          <Timeline.Connector>
            <Timeline.Separator />
            {displayStatusIcon}
          </Timeline.Connector>
          <Timeline.Content textStyle="xs">
            <Timeline.Title>
                <Badge colorPalette={color}>
                {workerStatus.status}
                </Badge>
            </Timeline.Title>
            <Timeline.Description>
                {workerStatus.additional_info} 
            </Timeline.Description>
          </Timeline.Content>
        </Timeline.Item>
    )
}


export function WorkerStatus() {

  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () =>  WorkerStatusService.getStatus(),
  });


  if (isLoading) {
    return <Box flex={1}>Loading...</Box>;
  }

  return (
    <Box flex={1}>
        <Timeline.Root size="sm">
            {data?.map((status, index) => (
                <TimelineEntry key={status.id} workerStatus={status} isLast={index === data.length - 1} />
            ))}
          </Timeline.Root>
    </Box>
  )
}

export function CollapsibleWorkerStatus() {
  const { data, isLoading } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () => WorkerStatusService.getStatus(),
  });
  const [isExpanded, setIsExpanded] = useState(false);

  if (isLoading) {
    return <Box flex={1}>Loading...</Box>;
  }

  if (!data || data.length === 0) {
    return <Box flex={1}>No status available</Box>;
  }

  const latestStatus = data[data.length - 1];

  return (
    <Box flex={1}>
      {isExpanded ? (
        <Box onClick={() => setIsExpanded(false)}>
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
      ) : (
        <Box 
          onClick={() => setIsExpanded(true)}
          cursor="pointer"
          p={2}
          border="1px solid gray"
          borderRadius="md"
        >
          <Timeline.Root size="sm">
            <TimelineEntry 
              workerStatus={latestStatus} 
              isLast={true} 
            />
          </Timeline.Root>
        </Box>
      )}
    </Box>
  );
}
