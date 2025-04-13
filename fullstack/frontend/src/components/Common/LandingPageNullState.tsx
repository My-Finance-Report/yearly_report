
import { Link } from "@tanstack/react-router";

import { Box, Heading, Text, VStack, HStack, Button, Spinner } from "@chakra-ui/react";
import { FaLink, FaUpload } from "react-icons/fa";
import { WorkerStatus } from "./WorkerStatus";
import { WorkerStatusService } from "@/client";
import { useQuery } from "@tanstack/react-query";

export function NullState({hasFetchedTransactions}: {hasFetchedTransactions: boolean}){

  const { data,isLoading } = useQuery({
    queryKey: ["currentStatus"],
    queryFn: () =>  WorkerStatusService.getStatus(),
    enabled: hasFetchedTransactions
  });

  const showWorkerProgress = data && data.length > 0 



  if (isLoading) {
    return (<Spinner/>);
  }
  if (showWorkerProgress && hasFetchedTransactions){
    return (
        <WorkerStatus/>
    )
  }

  return (

    <Box 
      p={8} 
      textAlign="center" 
      borderWidth="1px" 
      borderRadius="lg" 
      boxShadow="sm"
    >
      <VStack gap={6}>
        <Heading size="md">Welcome Aboard!</Heading>
        <Text >
          Get started by connecting accounts or uploading statements 
        </Text>
        <HStack gap={4} pt={4}>
          <Link to="/plaid">
            <Button 
              variant="solid"
            >
              <Box mr={2} display="inline-block"><FaLink /></Box>
              Link Accounts
            </Button>
          </Link>
          <Link to="/upload-files">
            <Button 
              variant="outline"
            >
              <Box mr={2} display="inline-block"><FaUpload /></Box>
              Upload Files
            </Button>
          </Link>
        </HStack>
      </VStack>
    </Box>
  )
}