import { Spinner, Flex } from "@chakra-ui/react";
export function PageSpinner(){
    return (
        <Flex justifyContent="center" alignItems="center" width="90vw" height="90vh">
            <Spinner size="lg" />
        </Flex>
    )
}