import {
  Box,
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
import Delete from "../../Common/DeleteAlert";
import { FiEdit, FiTrash } from "react-icons/fi";
import { TransactionKind } from "@/client";

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

function formatAmount(amount: number) {
  return amount.toLocaleString("en-US", {
    style: "currency",
    currency: "USD",
  });
}

export function ShowList({ widget }: { widget: NoCodeWidgetOut }) {
  const result = widget.result as Array<NoCodeTransactionOut>;

  const editTransactionModal = useDisclosure();
  const deleteTransactionModal = useDisclosure();

  return (
    <Box>
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
              <Delete
                type="transaction"
                isOpen={deleteTransactionModal.open}
                onClose={deleteTransactionModal.onClose}
                entity={data}
              />
            </TableRow>
          ))}
        </TableBody>
      </TableRoot>
    </Box>
  );
}
