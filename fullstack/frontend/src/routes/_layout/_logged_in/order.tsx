import { createFileRoute } from "@tanstack/react-router";
import {
  Box,
  Flex,
  Text,
  Button,
  Heading,
  NumberInput,
  Breadcrumb,
} from "@chakra-ui/react";
import { useState } from "react";
import { useQuery } from "@tanstack/react-query";
import {
  OrderableBase_Output,
  OrderBase_Input,
  OrderItemBase_Input,
  PosService,
  VariantBase_Output,
} from "@/client";
import useCustomToast from "@/hooks/useCustomToast";

type OrderItem = OrderItemBase_Input;

export const Route = createFileRoute("/_layout/_logged_in/order")({
  component: Order,
});

function QuantitySelector({
  setQuantity,
  quantity,
}: {
  setQuantity: React.Dispatch<React.SetStateAction<number>>;
  quantity: number;
}) {
  return (
    <Flex gap={2} alignItems="center">
      <Text fontSize="md" fontWeight="medium">
        Quantity
      </Text>
      <NumberInput.Root
        width="200px"
        value={String(quantity)}
        onValueChange={(e) => setQuantity(Number(e.value))}
      >
        <NumberInput.Control />
        <NumberInput.Input />
      </NumberInput.Root>
    </Flex>
  );
}

function OrderableCard({
  setInProgressOrder,
  orderable,
}: {
  setInProgressOrder: React.Dispatch<
    React.SetStateAction<OrderableBase_Output | null>
  >;
  orderable: OrderableBase_Output;
}) {
  return (
    <Box
      cursor="pointer"
      minH={100}
      onClick={() => setInProgressOrder(orderable)}
      display="flex"
      flexDirection="column"
      justifyContent="center"
      alignItems="center"
      p={4}
      minW={200}
      border="1px solid #ccc"
      borderRadius={4}
      _hover={{ backgroundColor: "gray.50" }}
    >
      <Text fontSize="lg" fontWeight="medium">
        {orderable.name}
      </Text>
      <Text color="gray.600" fontSize="sm">
        ${Number(orderable.price).toFixed(2)}
      </Text>
      {orderable.variantGroups.length > 0 && (
        <Text color="gray.500" fontSize="xs" mt={1}>
          {orderable.variantGroups.map((g) => g.name).join(", ")}
        </Text>
      )}
    </Box>
  );
}

function InOrderCard({
  setOrder,
  orderItem,
}: {
  setOrder: React.Dispatch<React.SetStateAction<OrderBase_Input>>;
  orderItem: OrderItem;
}) {
  const totalPrice =
    Number(orderItem.orderable.price) +
    orderItem.variants.reduce(
      (sum, variant) => sum + Number(variant.priceDelta),
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

  const variantsByGroup = orderItem.orderable.variantGroups
    .map((group) => {
      const variantsInGroup = orderItem.variants.filter(
        (selectedVariant) => selectedVariant.groupId === String(group.id),
      );
      const groupPriceDelta = variantsInGroup.reduce(
        (sum, v) => sum + Number(v.priceDelta),
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
      const key = `${group.groupName}-${String(v.id)}`;
      variantCounts.set(key, (variantCounts.get(key) || 0) + 1);
    });
  });

  return (
    <Box
      flex="row"
      p={4}
      minW={200}
      border="1px solid #ccc"
      borderRadius={4}
      display="flex"
      justifyContent="space-between"
      alignItems="flex-start"
    >
      <Flex direction="column" flex={1} gap={1}>
        <Text fontWeight="bold">{orderItem.orderable.name}</Text>
        {variantsByGroup.map((group) => {
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
            <Text key={group.groupName} color="gray.600" fontSize="sm">
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
        <Button size="sm" onClick={handleRemove}>
          Remove
        </Button>
      </Flex>
    </Box>
  );
}

function VariantGroupSelector({
  variantGroup,
  onSelect,
  onBack,
  onNext,
  currentSelections,
  isLastGroup,
}: {
  variantGroup: OrderableBase_Output["variantGroups"][0];
  onSelect: (variant: VariantBase_Output) => void;
  onBack: () => void;
  onNext: () => void;
  currentSelections: VariantBase_Output[];
  isLastGroup: boolean;
}) {
  const selectedVariantsInGroup = currentSelections;

  return (
    <Box p={4}>
      <Flex justify="space-between" mb={4}>
        <Box>
          <Heading size="md">{variantGroup.name}</Heading>
          {selectedVariantsInGroup.length > 0 && (
            <Text color="gray.600" fontSize="sm">
              Selected: {selectedVariantsInGroup.map((v) => v.name).join(", ")}
            </Text>
          )}
        </Box>
        <Flex gap={2}>
          <Button size="sm" onClick={onBack}>
            Back
          </Button>
          <Button
            size="sm"
            onClick={onNext}
            colorPalette={isLastGroup ? "green" : "gray"}
          >
            {isLastGroup ? "Add to Order" : "Next"}
          </Button>
        </Flex>
      </Flex>
      <Flex direction="column" gap={4}>
        {variantGroup.variants.map((variant) => {
          const count = selectedVariantsInGroup.filter(
            (v) => v.id === variant.id,
          ).length;
          return (
            <Box
              key={variant.id}
              p={4}
              border="1px solid #ccc"
              borderRadius={4}
              cursor="pointer"
              onClick={() => onSelect(variant)}
            >
              <Flex justify="space-between" mb={2}>
                <Flex gap={2} align="center">
                  <Text fontWeight="bold">{variant.name}</Text>
                  {count > 0 && <Text fontSize="sm">×{count}</Text>}
                </Flex>
                <Text>
                  $
                  {Number(variant.priceDelta) > 0
                    ? `+${Number(variant.priceDelta).toFixed(2)}`
                    : "0.00"}
                </Text>
              </Flex>
            </Box>
          );
        })}
      </Flex>
    </Box>
  );
}

function VariantSelector({
  orderable,
  setOrder,
  setInProgressOrder,
}: {
  orderable: OrderableBase_Output;
  setInProgressOrder: React.Dispatch<
    React.SetStateAction<OrderableBase_Output | null>
  >;
  setOrder: React.Dispatch<React.SetStateAction<OrderBase_Input>>;
}) {
  const [currentGroupIndex, setCurrentGroupIndex] = useState(0);
  const [quantity, setQuantity] = useState(1);
  const [variantsByGroup, setVariantsByGroup] = useState<
    Map<number, VariantBase_Output[]>
  >(new Map());

  if (orderable.variantGroups.length === 0) {
    return null;
  }

  const currentGroup = orderable.variantGroups[currentGroupIndex];

  const handleVariantSelect = (variant: VariantBase_Output) => {
    const currentVariants = variantsByGroup.get(currentGroup.id) || [];
    const variantIndex = currentVariants.findIndex((v) => v.id === variant.id);

    const newMap = new Map(variantsByGroup);
    if (variantIndex === -1) {
      newMap.set(currentGroup.id, [...currentVariants, variant]);
    } else {
      newMap.set(
        currentGroup.id,
        currentVariants.filter((v) => v.id !== variant.id),
      );
    }
    setVariantsByGroup(newMap);
  };

  const handleBack = () => {
    if (currentGroupIndex > 0) {
      setCurrentGroupIndex(currentGroupIndex - 1);
    } else {
      setInProgressOrder(null);
    }
  };

  const handleNext = () => {
    const currentVariants = variantsByGroup.get(currentGroup.id) || [];
    if (currentGroup.required && currentVariants.length === 0) {
      return; // Can't proceed if required group has no selection
    }

    const isLastGroup =
      currentGroupIndex === orderable.variantGroups.length - 1;
    if (isLastGroup) {
      // Collect all variants from all groups and convert to input format
      const allVariants = Array.from(variantsByGroup.entries()).flatMap(
        ([groupId, variants]) =>
          variants.map((variant) => ({
            id: String(variant.id),
            name: variant.name,
            priceDelta: variant.priceDelta,
            groupId: String(groupId),
          })),
      );

      // Add to order with proper types
      setOrder((prev) => ({
        ...prev,
        orderItems: [
          ...prev.orderItems,
          {
            orderable: orderable,
            variants: allVariants,
            quantity,
          },
        ],
      }));

      // Reset state
      setInProgressOrder(null);
      setVariantsByGroup(new Map());
      setCurrentGroupIndex(0);
      setQuantity(1);
    } else {
      // Move to next group
      setCurrentGroupIndex((i) => i + 1);
    }
  };

  return (
    <Box>
      <Box mb={4}>
        <Heading size="lg">{orderable.name}</Heading>
        <Text color="gray.600">
          Step {currentGroupIndex + 1} of {orderable.variantGroups.length}
        </Text>
      </Box>
      <VariantGroupSelector
        variantGroup={currentGroup}
        onSelect={handleVariantSelect}
        onBack={handleBack}
        onNext={handleNext}
        currentSelections={variantsByGroup.get(currentGroup.id) || []}
        isLastGroup={currentGroupIndex === orderable.variantGroups.length - 1}
      />
      {currentGroupIndex === orderable.variantGroups.length - 1 && (
        <Box mt={4}>
          <Flex gap={4} align="center" justify="center">
            <QuantitySelector setQuantity={setQuantity} quantity={quantity} />
          </Flex>
        </Box>
      )}
    </Box>
  );
}

function AllOrderables({
  setInProgressOrder,
}: {
  setInProgressOrder: React.Dispatch<
    React.SetStateAction<OrderableBase_Output | null>
  >;
}) {
  const { data: orderables } = useQuery({
    queryKey: ["orderables"],
    queryFn: () => PosService.getMenu(),
  });
  if (!orderables) return null;
  return (
    <Flex direction={"column"} gap={2}>
      {orderables.map((orderable) => (
        <OrderableCard
          setInProgressOrder={setInProgressOrder}
          key={orderable.id}
          orderable={orderable}
        />
      ))}
    </Flex>
  );
}

function Order() {
  const [order, setOrder] = useState<OrderBase_Input>({
    id: crypto.randomUUID(),
    timestamp: new Date().toISOString(),
    orderItems: [],
  });
  const toast = useCustomToast();
  const [inProgressOrder, setInProgressOrder] =
    useState<OrderableBase_Output | null>(null);

  const orderTotal = order.orderItems.reduce((sum, item) => {
    const itemPrice =
      Number(item.orderable.price) +
      item.variants.reduce(
        (variantSum, variant) => variantSum + Number(variant.priceDelta),
        0,
      );
    return sum + itemPrice * item.quantity;
  }, 0);

  const handleSubmitOrder = () => {
    PosService.createOrder({
      requestBody: order,
    });
    setOrder({
      id: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      orderItems: [],
    });
    toast(
      "Order created",
      "Your order has been successfully created.",
      "success",
    );
  };

  return (
    <Box p={4}>
      <BreadcrumbComponent />
      {inProgressOrder ? (
        <VariantSelector
          orderable={inProgressOrder}
          setOrder={setOrder}
          setInProgressOrder={setInProgressOrder}
        />
      ) : (
        <Box>
          <AllOrderables setInProgressOrder={setInProgressOrder} />
        </Box>
      )}

      {order.orderItems.length > 0 && (
        <Box mt={8}>
          <Heading size="md" mb={4}>
            Current Order
          </Heading>
          <Flex direction="column" gap={3}>
            {order.orderItems.map((orderItem, index) => (
              <InOrderCard
                key={`${orderItem.orderable.id}-${orderItem.variants.map((v) => v.id).join("-")}-${index}`}
                setOrder={setOrder}
                orderItem={orderItem}
              />
            ))}
            <Box
              p={4}
              borderRadius={4}
              display="flex"
              justifyContent="space-between"
            >
              <Text fontWeight="bold">Total</Text>
              <Text fontWeight="bold">${orderTotal.toFixed(2)}</Text>
            </Box>
            <Button
              mt={4}
              colorScheme="green"
              width="100%"
              size="lg"
              onClick={handleSubmitOrder}
            >
              Submit Order (${orderTotal.toFixed(2)})
            </Button>
          </Flex>
        </Box>
      )}
    </Box>
  );
}

function BreadcrumbComponent() {
  return (
    <Breadcrumb.Root size="lg">
      <Breadcrumb.List>
        <Breadcrumb.Item>
          <Breadcrumb.Link href="/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Order</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
