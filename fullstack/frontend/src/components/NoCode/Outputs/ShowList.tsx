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

interface NoCodeTransactionOut {
  amount: number;
  description: string;
  account_name: string;
  date_of_transaction: string;
  kind: string;
  category_name: string;
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
            {result.map((data, index) => {
              if (index === 0) {
                return Object.keys(data).map((key) => (
                  <TableCell key={key}>{key}</TableCell>
                ));
              }
            })}
          </TableRow>
        </TableHeader>
        <TableBody>
          {result.map((data, index) => (
            <TableRow key={index}>
              {Object.entries(data).map(([key, value]) => (
                <TableCell key={key}>{value}</TableCell>
              ))}
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
