import {
  Box,
  Text,
  Button,
  Flex,
  TableRoot,
  TableHeader,
  TableRow,
  TableCell,
  TableBody,
  Heading,
  useDisclosure,
} from "@chakra-ui/react";
import { NoCodeWidgetOut } from "@/client";

import EditTransaction from "../../Common/EditTransaction";
import DeleteAlert from "@/components/Common/DeleteAlert/DeleteAlert";
import { FiEdit, FiTrash } from "react-icons/fi";
import { TransactionKind } from "@/client";
import { EntityKind } from "@/components/Common/DeleteAlert/types";

interface NoCodeTransactionOut {
  id: number;
  category_id: number;
  amount: number;
  description: string;
  account_name: string;
  date_of_transaction: string;
  kind: TransactionKind;
  category_name: string;
}

function formatAmount(amount: number | undefined) {
  if (!amount) return "";
  return amount.toLocaleString("en-US", {
    style: "currency",
    currency: "USD",
  });
}

export function ShowList({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as Array<NoCodeTransactionOut>;

  const editTransactionModal = useDisclosure();
  const deleteTransactionModal = useDisclosure();

  if (result.length === 0) {
    return (
      <Box>
        <Heading>{widget.name}</Heading>
        <Flex
          direction="column"
          align="center"
          justify="center"
          py={8}
          px={4}
          borderWidth={1}
          borderRadius="md"
          borderStyle="dashed"
          textAlign="center"
        >
          <Text fontSize="lg" mb={1}>
            No transactions found
          </Text>
        </Flex>
      </Box>
    );
  }

  return (
    <Box
      overflow="auto"
      height="100%"
      width="100%"
      display="flex"
      flexDirection="column"
      minHeight={0}
    >
      <Heading flexShrink={0}>{widget.name}</Heading>
      <Box overflow="auto" flex="1" minHeight={0}>
        <TableRoot variant="outline" borderRadius="md" borderWidth={1}>
          <TableHeader>
            <TableRow>
              <TableCell>Category</TableCell>
              <TableCell>Description</TableCell>
              <TableCell>Amount</TableCell>
              <TableCell>Date of Transaction</TableCell>
              <TableCell>Kind</TableCell>
              <TableCell>Actions</TableCell>
            </TableRow>
          </TableHeader>
          <TableBody>
            {result.map((data, index) => (
              <TableRow key={index}>
                <TableCell>{data.category_name}</TableCell>
                <TableCell>{data.description}</TableCell>
                <TableCell>{formatAmount(data.amount)}</TableCell>
                <TableCell>{data.date_of_transaction}</TableCell>
                <TableCell>{data.kind}</TableCell>
                <TableCell>
                  <Flex direction="row" gap={2}>
                    <Button
                      onClick={editTransactionModal.onOpen}
                      size="sm"
                      variant="outline"
                    >
                      <FiEdit size="8px" />
                    </Button>
                    <Button
                      onClick={deleteTransactionModal.onOpen}
                      size="sm"
                      variant="outline"
                    >
                      <FiTrash size="8px" />
                    </Button>
                  </Flex>
                </TableCell>
                <EditTransaction
                  transaction={data}
                  isOpen={editTransactionModal.open}
                  onClose={editTransactionModal.onClose}
                />
                <DeleteAlert
                  isOpen={deleteTransactionModal.open}
                  onClose={deleteTransactionModal.onClose}
                  entity={{ ...data, kind: EntityKind.Transaction }}
                />
              </TableRow>
            ))}
          </TableBody>
        </TableRoot>
      </Box>
    </Box>
  );

  return (
    <Box overflow="auto" height="100%" width="100%">
      <Heading>{widget.name}</Heading>
      <TableRoot variant="outline" borderRadius="md" borderWidth={1}>
        <TableHeader>
          <TableRow>
            <TableCell>Category</TableCell>
            <TableCell>Description</TableCell>
            <TableCell>Amount</TableCell>
            <TableCell>Date of Transaction</TableCell>
            <TableCell>Kind</TableCell>
            <TableCell>Actions</TableCell>
          </TableRow>
        </TableHeader>
        <TableBody>
          {result.map((data, index) => (
            <TableRow key={index}>
              <TableCell>{data.category_name}</TableCell>
              <TableCell>{data.description}</TableCell>
              <TableCell>{formatAmount(data.amount)}</TableCell>
              <TableCell>{data.date_of_transaction}</TableCell>
              <TableCell>{data.kind}</TableCell>
              <TableCell>
                <Flex direction="row" gap={2}>
                  <Button
                    onClick={editTransactionModal.onOpen}
                    size="sm"
                    variant="outline"
                  >
                    <FiEdit size="8px" />
                  </Button>
                  <Button
                    onClick={deleteTransactionModal.onOpen}
                    size="sm"
                    variant="outline"
                  >
                    <FiTrash size="8px" />
                  </Button>
                </Flex>
              </TableCell>
              <EditTransaction
                transaction={data}
                isOpen={editTransactionModal.open}
                onClose={editTransactionModal.onClose}
              />
              <DeleteAlert
                isOpen={deleteTransactionModal.open}
                onClose={deleteTransactionModal.onClose}
                entity={{ ...data, kind: EntityKind.Transaction }}
              />
            </TableRow>
          ))}
        </TableBody>
      </TableRoot>
    </Box>
  );
}
