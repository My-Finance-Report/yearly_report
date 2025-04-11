import { WorkerStatusService, WorkerStatusOut } from "@/client";
import {Badge,Box,  Timeline, Avatar, Span} from '@chakra-ui/react'
import { useQuery } from "@tanstack/react-query";
import { LuCheck } from "react-icons/lu";


export function WorkerStatus() {

  const { data } = useQuery<WorkerStatusOut[], Error>({
    queryKey: ["currentStatus"],
    queryFn: () =>  WorkerStatusService.getStatus(),
  });

  return (
    <Box flex={1}>
        <Timeline.Root size="sm">
            <Timeline.Item>
              <Timeline.Connector>
                <Timeline.Separator />
                <Timeline.Indicator>
                  <Avatar.Root size="full">
                    <Avatar.Image src="https://bit.ly/sage-adebayo" />
                    <Avatar.Fallback name="Sage" />
                  </Avatar.Root>
                </Timeline.Indicator>
              </Timeline.Connector>
              <Timeline.Content textStyle="xs">
                <Timeline.Title>
                  <Span fontWeight="medium">sage</Span>
                  created a new project
                </Timeline.Title>
              </Timeline.Content>
            </Timeline.Item>
            <Timeline.Item>
              <Timeline.Connector>
                <Timeline.Separator />
                <Timeline.Indicator>
                  <LuCheck />
                </Timeline.Indicator>
              </Timeline.Connector>
              <Timeline.Content textStyle="xs">
                <Timeline.Title>
                  <Span fontWeight="medium">sage</Span>
                  changed status from <Badge size="sm">
                    In progress
                  </Badge> to{" "}
                  <Badge colorPalette="teal" size="sm">
                    Completed
                  </Badge>
                </Timeline.Title>
              </Timeline.Content>
            </Timeline.Item>
          </Timeline.Root>
    </Box>
  )
}
