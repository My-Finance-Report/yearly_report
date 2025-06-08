import React from "react";
import { Center, Spinner, Box } from "@chakra-ui/react";

/**
 * A general-purpose page loader that centers a spinner in the viewport.
 * Use this component wherever you need to indicate a full-page loading state.
 */
const PageLoader: React.FC = () => (
  <Center minH="60vh" w="100%">
    <Box>
      <Spinner size="xl" />
    </Box>
  </Center>
);

export default PageLoader;
