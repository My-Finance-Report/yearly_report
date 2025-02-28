import { Button, Flex } from "@chakra-ui/react"

type PaginationFooterProps = {
  hasNextPage?: boolean
  hasPreviousPage?: boolean
  onChangePage: (newPage: number) => void
  page: number
}

export function PaginationFooter({
  hasNextPage,
  hasPreviousPage,
  onChangePage,
  page,
}: PaginationFooterProps) {
  return (
    <Flex
      gap={4}
      alignItems="center"
      mt={4}
      direction="row"
      justifyContent="flex-end"
    >
      <Button
        onClick={() => onChangePage(page - 1)}
        disabled={!hasPreviousPage || page <= 1}
      >
        Previous
      </Button>
      <span>Page {page}</span>
      <Button disabled={!hasNextPage} onClick={() => onChangePage(page + 1)}>
        Next
      </Button>
    </Flex>
  )
}
