import { WorkerStatusService, WorkerStatusOut } from "@/client";
import {Timeline} from '@chakra-ui/react'
import { useQuery } from "@tanstack/react-query";


export function WorkerStatus() {

  const { data } = useQuery<WorkerStatusOut, Error>({
    queryKey: ["currentStatus"],
    queryFn: async () => {
      try {
        return await WorkerStatusService.status();
      } catch {
        return null;
      }
    },
  });


  return (
   <Timeline>
   </Timeline>
  )
}

