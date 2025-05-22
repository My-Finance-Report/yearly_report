import { Box, Flex, Button, Text } from "@chakra-ui/react";
import { OrderBase_Input, OrderItemBase_Input } from "@/client";

type OrderItem = OrderItemBase_Input;

function calcTotal(order: OrderBase_Input) {
  const orderTotal = order.orderItems.reduce((sum, item) => {
    const itemPrice =
      Number(item.orderable.price) +
      item.variants.reduce(
        (variantSum, variant) => variantSum + Number(variant.price_delta),
        0,
      );
    return sum + itemPrice * item.quantity;
  }, 0);

  return orderTotal;
}

export function OrderCard({
  order,
  setOrder,
  allowEdits,
}: {
  order: OrderBase_Input;
  setOrder: React.Dispatch<React.SetStateAction<OrderBase_Input>>;
  allowEdits?: boolean;
}) {
  return (
    <Flex direction="column" gap={3} borderRadius={4}>
      {order.orderItems.map((orderItem, index) => (
        <InOrderCard
          key={`${orderItem.orderable.id}-${orderItem.variants.map((v) => v.id).join("-")}-${index}`}
          setOrder={setOrder}
          orderItem={orderItem}
          allowEdits={allowEdits}
        />
      ))}
      <Box p={4} borderRadius={4} display="flex" justifyContent="space-between">
        <Text fontWeight="bold">Total</Text>
        <Text fontWeight="bold">${calcTotal(order).toFixed(2)}</Text>
      </Box>
    </Flex>
  );
}

function InOrderCard({
  setOrder,
  orderItem,
  allowEdits,
}: {
  setOrder: React.Dispatch<React.SetStateAction<OrderBase_Input>>;
  orderItem: OrderItem;
  allowEdits?: boolean;
}) {
  const totalPrice =
    Number(orderItem.orderable.price) +
    orderItem.variants.reduce(
      (sum, variant) => sum + Number(variant.price_delta),
      0,
    );

  const handleRemove = () => {
    setOrder((prev) => ({
      ...prev,
      orderItems: prev.orderItems.filter(
        (item) =>
          !(
            item.orderable.id === orderItem.orderable.id &&
            JSON.stringify(item.variants) === JSON.stringify(orderItem.variants)
          ),
      ),
    }));
  };

  const variantsByGroup = orderItem.orderable.variant_groups
    .map((group) => {
      const variantsInGroup = orderItem.variants.filter(
        (selectedVariant) => selectedVariant.group_id === group.id,
      );
      const groupPriceDelta = variantsInGroup.reduce(
        (sum, v) => sum + Number(v.price_delta),
        0,
      );
      return {
        groupName: group.name,
        variants: variantsInGroup,
        priceDelta: groupPriceDelta,
      };
    })
    .filter((group) => group.variants.length > 0);

  const variantCounts = new Map<string, number>();
  variantsByGroup.forEach((group) => {
    group.variants.forEach((v) => {
      const key = `${group.groupName}-${v.id}`;
      variantCounts.set(key, (variantCounts.get(key) || 0) + 1);
    });
  });

  console.log(variantsByGroup)

  return (
    <Box
      flex="row"
      p={4}
      minW="90%"
      border="1px solid #ccc"
      borderRadius={4}
      display="flex"
      justifyContent="space-between"
      alignItems="flex-start"
    >
      <Flex direction="column" flex={1} gap={1}>
        <Text fontWeight="bold">{orderItem.orderable.name}</Text>
        {variantsByGroup.map((group, index) => {
          const consolidatedVariants = new Map<
            string,
            { name: string; count: number }
          >();
          group.variants.forEach((v) => {
            const existing = consolidatedVariants.get(String(v.id));
            if (existing) {
              existing.count++;
            } else {
              consolidatedVariants.set(String(v.id), {
                name: v.name,
                count: 1,
              });
            }
          });

          return (
            <Text
              key={`${index}-${group.groupName}`}
              color="gray.600"
              fontSize="sm"
            >
              {group.groupName}:{" "}
              {Array.from(consolidatedVariants.values())
                .map((v) => (v.count > 1 ? `${v.name} ×${v.count}` : v.name))
                .join(", ")}
              {group.priceDelta > 0 && (
                <Text as="span" color="gray.400" ml={2}>
                  (+${group.priceDelta.toFixed(2)})
                </Text>
              )}
            </Text>
          );
        })}
      </Flex>
      <Flex align="center" gap={4}>
        <Flex direction="column" align="flex-end">
          <Text>
            ${totalPrice.toFixed(2)} × {orderItem.quantity}
          </Text>
        </Flex>
        {allowEdits && (
          <Button size="sm" onClick={handleRemove}>
            Remove
          </Button>
        )}
      </Flex>
    </Box>
  );
}
