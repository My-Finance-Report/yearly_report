import { createFileRoute, useNavigate } from "@tanstack/react-router";
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
  OrderBase_Input,
  OrderableOutput_Output,
  PosService,
  VariantBase_Output,
} from "@/client";
import { OrderCard } from "@/components/Pos/Order";
import useCustomToast from "@/hooks/useCustomToast";

export const Route = createFileRoute("/_layout/_logged_in/pos/order")({
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
        width="90%"
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
    React.SetStateAction<OrderableOutput_Output | null>
  >;
  orderable: OrderableOutput_Output;
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
      minW="90%"
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
      {orderable.variant_groups.length > 0 && (
        <Text color="gray.500" fontSize="xs" mt={1}>
          {orderable.variant_groups.map((g) => g.name).join(", ")}
        </Text>
      )}
    </Box>
  );
}

function VariantGroupSelector({
  variantGroup,
  onSelect,
  onDeselect,
  onBack,
  onNext,
  currentSelections,
  isLastGroup,
}: {
  variantGroup: OrderableOutput_Output["variant_groups"][0];
  onSelect: (variant: VariantBase_Output) => void;
  onDeselect: (variant: VariantBase_Output) => void;
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
                  {count > 0 && (
                    <Button
                      size="sm"
                      onClick={(e) => {
                        e.stopPropagation();
                        onDeselect(variant);
                      }}
                      colorPalette="gray"
                    >
                      Remove
                    </Button>
                  )}
                </Flex>
                <Text>
                  $
                  {Number(variant.price_delta) > 0
                    ? `+${Number(variant.price_delta).toFixed(2)}`
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
  orderable: OrderableOutput_Output;
  setInProgressOrder: React.Dispatch<
    React.SetStateAction<OrderableOutput_Output | null>
  >;
  setOrder: React.Dispatch<React.SetStateAction<OrderBase_Input>>;
}) {
  const [currentGroupIndex, setCurrentGroupIndex] = useState(0);
  const [quantity, setQuantity] = useState(1);
  const [variantsByGroup, setVariantsByGroup] = useState<
    Map<number, Map<number, number>>
  >(new Map());

  if (orderable.variant_groups.length === 0) {
    return null;
  }

  const currentGroup = orderable.variant_groups[currentGroupIndex];

  const handleVariantSelect = (variant: VariantBase_Output) => {
    if (!currentGroup?.id || !variant?.id) return;

    const currentVariantCounts =
      variantsByGroup.get(currentGroup.id) || new Map<number, number>();
    const currentCount = currentVariantCounts.get(variant.id) || 0;

    const newVariantCounts = new Map(currentVariantCounts);

    newVariantCounts.set(variant.id, currentCount + 1);

    const newMap = new Map(variantsByGroup);
    newMap.set(currentGroup.id, newVariantCounts);
    setVariantsByGroup(newMap);
  };

  const handleVariantDeselect = (variant: VariantBase_Output) => {
    if (!currentGroup?.id || !variant?.id) return;

    const currentVariantCounts =
      variantsByGroup.get(currentGroup.id) || new Map<number, number>();
    const currentCount = currentVariantCounts.get(variant.id) || 0;

    const newVariantCounts = new Map(currentVariantCounts);
    newVariantCounts.set(variant.id, currentCount - 1);

    const newMap = new Map(variantsByGroup);
    newMap.set(currentGroup.id, newVariantCounts);
    setVariantsByGroup(newMap);
  };

  const handleBack = () => {
    if (currentGroupIndex > 0) {
      setCurrentGroupIndex(currentGroupIndex - 1);
    } else {
      setInProgressOrder(null);
    }
  };

  const getVariantsForGroup = (groupId: number): VariantBase_Output[] => {
    const variantCounts =
      variantsByGroup.get(groupId) || new Map<number, number>();
    const group = orderable.variant_groups.find((g) => g.id === groupId);
    if (!group) return [];

    return Array.from(variantCounts.entries()).flatMap(([variantId, count]) => {
      const variant = group.variants.find((v) => v.id === variantId);
      return variant ? Array(count).fill(variant) : [];
    });
  };

  const handleNext = () => {
    const variants_data = getVariantsForGroup(currentGroup.id);
    if (currentGroup.required && variants_data.length === 0) {
      return;
    }

    const isLastGroup =
      currentGroupIndex === orderable.variant_groups.length - 1;
    if (isLastGroup) {
      const allVariants = Array.from(variantsByGroup.keys()).flatMap(
        (groupId) =>
          getVariantsForGroup(groupId).map((variant: VariantBase_Output) => ({
            id: variant.id!,
            name: variant.name,
            price_delta: variant.price_delta,
            group_id: groupId,
          })),
      );

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

      setInProgressOrder(null);
      setVariantsByGroup(new Map());
      setCurrentGroupIndex(0);
      setQuantity(1);
    } else {
      setCurrentGroupIndex((i) => i + 1);
    }
  };

  return (
    <Box>
      <Box mb={4}>
        <Heading size="lg">{orderable.name}</Heading>
        <Text color="gray.600">
          Step {currentGroupIndex + 1} of {orderable.variant_groups.length}
        </Text>
      </Box>
      <VariantGroupSelector
        variantGroup={currentGroup}
        onSelect={handleVariantSelect}
        onDeselect={handleVariantDeselect}
        onBack={handleBack}
        onNext={handleNext}
        currentSelections={getVariantsForGroup(currentGroup.id)}
        isLastGroup={currentGroupIndex === orderable.variant_groups.length - 1}
      />
      {currentGroupIndex === orderable.variant_groups.length - 1 && (
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
    React.SetStateAction<OrderableOutput_Output | null>
  >;
}) {
  const { data: orderables } = useQuery({
    queryKey: ["orderables"],
    queryFn: () => PosService.getMenu(),
  });
  if (!orderables) return null;
  return (
    <Flex direction={"column"} gap={10} p={10}>
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

export function Order({
  includeBreadcrumb = true,
}: {
  includeBreadcrumb?: boolean;
}) {
  console.log("rendering order");
  const [order, setOrder] = useState<OrderBase_Input>({
    id: crypto.randomUUID(),
    timestamp: new Date().toISOString(),
    orderItems: [],
  });
  const toast = useCustomToast();
  const navigate = useNavigate();
  const [inProgressOrder, setInProgressOrder] =
    useState<OrderableOutput_Output | null>(null);

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
    navigate({ to: "/pos/recent-orders/" });
  };

  return (
    <Box p={4}>
      {includeBreadcrumb && <BreadcrumbComponent />}
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
          <OrderCard order={order} setOrder={setOrder} allowEdits />
          <Button
            mt={4}
            colorScheme="green"
            width="100%"
            size="lg"
            onClick={handleSubmitOrder}
          >
            Submit Order
          </Button>
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
          <Breadcrumb.Link href="/pos/pos">Home</Breadcrumb.Link>
        </Breadcrumb.Item>
        <Breadcrumb.Separator />
        <Breadcrumb.Item>
          <Breadcrumb.CurrentLink>Order</Breadcrumb.CurrentLink>
        </Breadcrumb.Item>
      </Breadcrumb.List>
    </Breadcrumb.Root>
  );
}
