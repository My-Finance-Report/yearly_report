import {
  Badge,
  Box,
  Container,
  Flex,
  Heading,
  Skeleton,
  Table,
} from "@chakra-ui/react";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { createFileRoute, useNavigate } from "@tanstack/react-router";
import { useEffect } from "react";
import { z } from "zod";

import { type UserOut, UsersService } from "@/client";
import AddUser from "@/components/Admin/AddUser";
import ActionsMenu from "@/components/Common/ActionsMenu";
import Navbar from "@/components/Common/Navbar";
import { PaginationFooter } from "@/components/Common/PaginationFooter.tsx";

const usersSearchSchema = z.object({
  page: z.number().catch(1),
});

export const Route = createFileRoute("/_layout/_logged_in/admin")({
  component: Admin,
  validateSearch: (search) => usersSearchSchema.parse(search),
});

const PER_PAGE = 5;

function getUsersQueryOptions({ page }: { page: number }) {
  return {
    queryFn: () =>
      UsersService.readUsers({ skip: (page - 1) * PER_PAGE, limit: PER_PAGE }),
    queryKey: ["users", { page }],
  };
}

function UsersTable() {
  const queryClient = useQueryClient();
  const currentUser = queryClient.getQueryData<UserOut>(["currentUser"]);
  const { page } = Route.useSearch();
  const navigate = useNavigate({ from: Route.fullPath });
  const setPage = (page: number) =>
    navigate({
      search: (prev: { [key: string]: string }) => ({ ...prev, page }),
    });

  const {
    data: users,
    isPending,
    isPlaceholderData,
  } = useQuery({
    ...getUsersQueryOptions({ page }),
    placeholderData: (prevData) => prevData,
  });

  const hasNextPage = !isPlaceholderData && users?.data.length === PER_PAGE;
  const hasPreviousPage = page > 1;

  useEffect(() => {
    if (hasNextPage) {
      queryClient.prefetchQuery(getUsersQueryOptions({ page: page + 1 }));
    }
  }, [page, queryClient, hasNextPage]);

  return (
    <>
      <Box overflowX="auto">
        <Table.Root variant="outline">
          <Table.Header>
            <Table.Row>
              <Table.ColumnHeader>Full name</Table.ColumnHeader>
              <Table.ColumnHeader>Email</Table.ColumnHeader>
              <Table.ColumnHeader textAlign="center">Role</Table.ColumnHeader>
              <Table.ColumnHeader textAlign="center">Status</Table.ColumnHeader>
              <Table.ColumnHeader textAlign="center">
                Actions
              </Table.ColumnHeader>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {isPending ? (
              <Table.Row>
                {new Array(5).fill(null).map((_, index) => (
                  <Table.Cell key={index.toString()}>
                    <Skeleton maxLines={3} paddingBlock="16px" />
                  </Table.Cell>
                ))}
              </Table.Row>
            ) : (
              users?.data.map((user) => (
                <Table.Row key={user.email}>
                  <Table.Cell
                    color={!user.full_name ? "gray.500" : "inherit"}
                    truncate
                  >
                    {user.full_name || "N/A"}
                    {currentUser?.id === user.id && (
                      <Badge ml="1" colorScheme="teal">
                        You
                      </Badge>
                    )}
                  </Table.Cell>
                  <Table.Cell truncate>{user.email}</Table.Cell>
                  <Table.Cell textAlign="center">
                    {user.is_superuser ? "Superuser" : "User"}
                  </Table.Cell>
                  <Table.Cell textAlign="center">
                    <Flex gap={2} justifyContent="center">
                      <Box
                        w="2"
                        h="2"
                        borderRadius="50%"
                        bg={user.is_active ? "green.500" : "red.500"}
                      />
                      {user.is_active ? "Active" : "Inactive"}
                    </Flex>
                  </Table.Cell>
                  <Table.Cell textAlign="center">
                    <ActionsMenu
                      type="user"
                      disabled={currentUser?.id === user.id}
                      entity={user}
                    />
                  </Table.Cell>
                </Table.Row>
              ))
            )}
            {!isPending && (
              <Table.Row fontWeight="bold">
                <Table.Cell colSpan={5} textAlign="center">
                  Showing {users?.data.length || 0} users
                </Table.Cell>
              </Table.Row>
            )}
          </Table.Body>
        </Table.Root>
      </Box>
      <PaginationFooter
        onChangePage={setPage}
        page={page}
        hasNextPage={hasNextPage}
        hasPreviousPage={hasPreviousPage}
      />
    </>
  );
}

function Admin() {
  return (
    <Container maxW="full">
      <Heading size="lg" textAlign={{ base: "center", md: "left" }} pt={12}>
        Users Management
      </Heading>

      <Navbar type={"User"} addModalAs={AddUser} />
      <UsersTable />
    </Container>
  );
}
