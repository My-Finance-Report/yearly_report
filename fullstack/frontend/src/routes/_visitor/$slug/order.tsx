import { Availability, PosService, ShopOut } from "@/client";
import { DumbSelect } from "@/components/ui/dumb-select";
import { Order } from "@/routes/_layout/_logged_in/pos/order";
import {
  Button,
  Container,
  Heading,
  VStack,
  Text,
  Card,
  CardBody,
  CardHeader,
  Stack,
  Input,
  HStack,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { useState } from "react";

export const Route = createFileRoute("/_visitor/$slug/order")({
  component: RouteComponent,
});

type PickupTime = {
  timestamp: string;
  label: string;
};
enum Page {
  GuestInfo,
  PickupTime,
  Order,
}

function makeRanges(times: Availability[]): PickupTime[] {
  const ranges: PickupTime[] = [];
  const FIFTEEN_MINUTES = 15 * 60 * 1000;

  times.forEach((timeRange) => {
    const start = new Date(timeRange.open_time);
    const end = new Date(timeRange.close_time);

    const startMinutes = start.getMinutes();
    const roundedMinutes = Math.ceil(startMinutes / 15) * 15;
    start.setMinutes(roundedMinutes, 0, 0);

    const endMinutes = end.getMinutes();
    const flooredMinutes = Math.floor(endMinutes / 15) * 15;
    end.setMinutes(flooredMinutes, 0, 0);

    let current = new Date(start);
    while (current <= end) {
      const hours = current.getHours();
      const minutes = current.getMinutes();
      const period = hours >= 12 ? "PM" : "AM";
      const displayHours = hours % 12 || 12;

      ranges.push({
        timestamp: current.toISOString(),
        label: `${displayHours}:${minutes.toString().padStart(2, "0")} ${period}`,
      });

      current = new Date(current.getTime() + FIFTEEN_MINUTES);
    }
  });

  return ranges;
}

function RouteComponent() {
  const [selectedTime, setSelectedTime] = useState<PickupTime | null>(null);
  const [guestInfo, setGuestInfo] = useState({
    name: "",
    phone: "",
    email: "",
  });
  const { slug } = Route.useParams();

  const { data: shop } = useQuery({
    queryKey: ["shop", slug],
    queryFn: () => PosService.getShop({ slug }),
  });

  const orderFlow = [Page.GuestInfo, Page.PickupTime, Page.Order];
  const [currentPage, setCurrentPage] = useState<Page>(Page.GuestInfo);

  const handleNext = () => {
    setCurrentPage(orderFlow[orderFlow.indexOf(currentPage) + 1]);
  };

  const handleBack = () => {
    setCurrentPage(orderFlow[orderFlow.indexOf(currentPage) - 1]);
  };

  if (!shop) {
    return (
      <Container maxW="container.xl" py={8}>
        <Text>Shop not found</Text>
      </Container>
    );
  }

  const renderCurrentPage = () => {
    switch (currentPage) {
      case Page.GuestInfo:
        return <GuestInfo guestInfo={guestInfo} setGuestInfo={setGuestInfo} />;
      case Page.PickupTime:
        return (
          <SelectDateTime
            selectedTime={selectedTime}
            onTimeSelect={setSelectedTime}
            times={makeRanges(shop?.availability ?? [])}
          />
        );
      case Page.Order:
        return <Order includeBreadcrumb={false} />;
    }
  };

  return (
    <Container maxW="container.xl" py={8}>
      <VStack align="stretch">
        <AccountHeader
          shop={shop}
          guestInfo={guestInfo}
          pickupTime={selectedTime}
        />
        {renderCurrentPage()}
        <HStack>
          {currentPage > Page.GuestInfo && (
            <Button onClick={handleBack}>Back</Button>
          )}
          {currentPage < Page.Order && (
            <Button onClick={handleNext}>Next</Button>
          )}
        </HStack>
      </VStack>
    </Container>
  );
}

function AccountHeader({
  shop,
  guestInfo,
  pickupTime,
}: {
  shop: ShopOut;
  guestInfo: { name: string; phone: string; email: string };
  pickupTime: PickupTime | null;
}) {
  return (
    <Card.Root>
      <CardHeader>
        <Heading size="lg">{shop.name}</Heading>
      </CardHeader>
      <CardBody>
        {guestInfo.name && <Text>Order for {guestInfo.name} :)</Text>}
        {pickupTime && <Text>Picking up at {pickupTime.label}</Text>}
      </CardBody>
    </Card.Root>
  );
}

function GuestInfo({
  guestInfo,
  setGuestInfo,
}: {
  guestInfo: {
    name: string;
    phone: string;
    email: string;
  };
  setGuestInfo: (info: { name: string; phone: string; email: string }) => void;
}) {
  return (
    <Card.Root>
      <CardHeader>
        <Heading size="md">Guest Information</Heading>
      </CardHeader>
      <CardBody>
        <Stack spaceY={4}>
          <Text>Please enter your information:</Text>
          <Input
            placeholder="Name"
            value={guestInfo.name}
            onChange={(e) =>
              setGuestInfo({ ...guestInfo, name: e.target.value })
            }
          />
          <Input
            placeholder="Phone"
            value={guestInfo.phone}
            onChange={(e) =>
              setGuestInfo({ ...guestInfo, phone: e.target.value })
            }
          />
        </Stack>
      </CardBody>
    </Card.Root>
  );
}

interface SelectDateTimeProps {
  selectedTime: PickupTime | null;
  onTimeSelect: (time: PickupTime) => void;
  times: PickupTime[];
}

function SelectDateTime({
  selectedTime,
  onTimeSelect,
  times,
}: SelectDateTimeProps) {
  console.log(times);

  return (
    <Card.Root>
      <CardHeader>
        <Heading size="md">Select Pickup Time</Heading>
      </CardHeader>
      <CardBody>
        <Stack spaceY={4}>
          <Text>Please select your preferred pickup time:</Text>
          <DumbSelect
            placeholder="Choose a time"
            selectedOption={selectedTime}
            setSelectedOption={onTimeSelect}
            labelExtractor={(time: PickupTime) => time.label}
            keyExtractor={(time: PickupTime) => time.timestamp}
            options={times}
          />
        </Stack>
      </CardBody>
    </Card.Root>
  );
}
