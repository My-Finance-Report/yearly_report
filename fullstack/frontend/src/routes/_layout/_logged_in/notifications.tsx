import { useQuery, useMutation } from "@tanstack/react-query";
import { createFileRoute } from "@tanstack/react-router";
import { EffectOut, NoCodeService } from "@/client";
import {
  Text,
  Flex,
  Input,
  Box,
  Heading,
  Stack,
  HStack,
  Badge,
  Icon,
  Spacer,
  Button,
  Tooltip,
  Separator,
  Tabs,
  FieldRoot,
  FieldLabel,
  Textarea,
  Spinner,
} from "@chakra-ui/react";
import { Card, CardHeader, CardContent, CardFooter } from "@/components/ui/card";
import useCustomToast from "@/hooks/useCustomToast";
import { useRef, useEffect } from "react";
import { FaBell, FaEnvelope, FaEdit, FaToggleOn, FaPlay } from "react-icons/fa";
import { useState } from "react";

export const Route = createFileRoute("/_layout/_logged_in/notifications")({
  component: NotificationComponent,
});

function NotificationComponent() {
  const { data, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  return (
    <Box maxW="container.lg" py={8}>
      <Box mb={8} p={6} borderRadius="lg" shadow="md">
        <Flex align="center" mb={4}>
          <Icon as={FaBell} fontSize="2xl" color="blue.500" mr={3} />
          <Heading size="lg">Notification Settings</Heading>
          <Spacer />
          <Button colorScheme="blue" size="sm">
            <Icon as={FaEdit} mr={2} />
            Add New
          </Button>
        </Flex>
        <Text>
          Configure how and when you want to be notified about your financial
          activities.
        </Text>
      </Box>

      <Tabs.Root defaultValue="notifications">
        <Tabs.List>
          <Tabs.Trigger value="notifications">
            <Icon as={FaBell} mr={2} />
            My Notifications
          </Tabs.Trigger>
          <Tabs.Trigger value="preview">
            <Icon as={FaPlay} mr={2} />
            Test & Preview
          </Tabs.Trigger>
        </Tabs.List>
        <Tabs.Content value="notifications" pt={4}>
          {isLoading ? (
            <Flex justify="center" p={8}>
              <Spinner />
              <Text ml={3}>Loading your notification settings...</Text>
            </Flex>
          ) : (
            <Stack gap={6} align="stretch">
              {data?.map((effect, index) => (
                <ShowEffect key={index} effect={effect} />
              ))}
            </Stack>
          )}
        </Tabs.Content>
        <Tabs.Content value="preview" pt={4}>
          <NotificationTester />
        </Tabs.Content>
      </Tabs.Root>
    </Box>
  );
}

function ShowEffect({ effect }: { effect: EffectOut }) {
  return (
    <Card className="overflow-hidden shadow-md">
      <Flex p={4} borderBottomWidth="1px" align="center">
        <Icon
          as={effect.effect_type === "email" ? FaEnvelope : FaBell}
          color={effect.effect_type === "email" ? "blue.500" : "purple.500"}
          mr={3}
          fontSize="xl"
        />
        <Heading size="md">{effect.name}</Heading>
        <Spacer />
        <Badge
          colorScheme="green"
          variant="subtle"
          px={2}
          py={1}
          borderRadius="full"
        >
          Active
        </Badge>
        <Tooltip.Root>
          <Tooltip.Trigger asChild>
            <Button variant="ghost" size="sm" ml={2}>
              <Icon as={FaToggleOn} color="green.500" fontSize="lg" />
            </Button>
          </Tooltip.Trigger>
          <Tooltip.Content>Toggle notification status</Tooltip.Content>
        </Tooltip.Root>
      </Flex>

      <CardContent>
        <Stack gap={4} align="stretch">
          <Box>
            <Flex p={3} borderRadius="md" direction="column" gap={2}>
              <Text fontWeight="medium" fontSize="sm">
                Trigger Condition:
              </Text>
              <NotificationCondition effect={effect} />

              <Separator orientation="horizontal" my={2} />

              <HStack gap={3}>
                <Badge colorScheme="blue" variant="subtle" px={2} py={1}>
                  {effect.effect_type === "email"
                    ? "Email Notification"
                    : "In-app Notification"}
                </Badge>
                <Text fontSize="sm">
                  Frequency: At most once per {effect.config.frequency_days}{" "}
                  day(s)
                </Text>
              </HStack>
            </Flex>
          </Box>

          <Box mt={2}>
            <Text fontWeight="medium" fontSize="sm" mb={2}>
              Email Template:
            </Text>
            <NotificationPreview
              subject={effect.config.subject}
              template={effect.config.template}
            />
          </Box>
        </Stack>
      </CardContent>

      <CardFooter className="justify-end border-t">
        <Button
          size="sm"
          colorScheme="red"
          variant="outline"
        >
          <Icon as={FaEdit} mr={2} />
          Delete
        </Button>
      </CardFooter>
    </Card>
  );
}

function NumberInput({ value }: { value: number }) {
  return (
    <Input
      type="number"
      defaultValue={value}
      width="70px"
      mx={2}
      textAlign="center"
      fontWeight="medium"
      _hover={{ borderColor: "blue.400" }}
    />
  );
}

function getStatement(effect: EffectOut) {
  switch (effect.condition) {
    case "amount_over": {
      const amount = effect.conditional_parameters["amount"] as number;
      const comparator = effect.conditional_parameters["comparator"] as string;
      return (
        <Flex direction={"row"} align="center" wrap="wrap" fontWeight="medium">
          <Text>If a transaction with an amount</Text>
          <Text mx={1} fontWeight="bold" color="blue.500">
            {comparator}
          </Text>
          <NumberInput value={amount} />
          <Text>was uploaded</Text>
        </Flex>
      );
    }
    case "count_of_transactions": {
      const count = effect.conditional_parameters["count"] as number;
      const comparator = effect.conditional_parameters["comparator"] as string;
      return (
        <Flex direction={"row"} align="center" wrap="wrap" fontWeight="medium">
          <Text>If</Text>
          <Text mx={1} fontWeight="bold" color="blue.500">
            {comparator}
          </Text>
          <NumberInput value={count} />
          <Text>transactions are uploaded</Text>
        </Flex>
      );
    }
    default: {
      throw new Error("not implemented");
    }
  }
}

function NotificationCondition({ effect }: { effect: EffectOut }) {
  return getStatement(effect);
}

function NotificationPreview({
  subject,
  template,
  html,
}: {
  subject: string;
  template?: string;
  html?: string;
}) {
  const iframeRef = useRef<HTMLIFrameElement>(null);

  useEffect(() => {
    if (html && iframeRef.current) {
      // Get the iframe document
      const iframeDoc = iframeRef.current.contentDocument;
      if (!iframeDoc) return;

      // Write the HTML content to the iframe
      iframeDoc.open();
      iframeDoc.write(`
        <!DOCTYPE html>
        <html>
          <head>
            <style>
              body { 
                font-family: system-ui, sans-serif; 
                margin: 0; 
                padding: 0; 
                color: #333;
              }
              table { 
                border-collapse: collapse; 
                width: 100%; 
                margin: 16px 0;
              }
              th, td { 
                border: 1px solid #ddd; 
                padding: 8px; 
                text-align: left;
              }
              th { 
                background-color: #f5f5f5; 
              }
              .positive { 
                color: green; 
              }
              .negative { 
                color: red; 
              }
            </style>
          </head>
          <body>
            ${html}
          </body>
        </html>
      `);
      iframeDoc.close();
    }
  }, [html]);

  return (
    <Card className="border">
      <Box p={3} borderBottomWidth="1px" fontWeight="medium" bg="gray.50">
        {subject}
      </Box>
      {html ? (
        <CardContent className="p-0 overflow-hidden">
          <iframe 
            ref={iframeRef} 
            title="Email Preview" 
            className="w-full border-0 min-h-[300px]" 
            sandbox="allow-same-origin"
          />
        </CardContent>
      ) : (
        <CardContent>{template}</CardContent>
      )}
    </Card>
  );
}

function NotificationTester() {
  const showToast = useCustomToast();
  const [formData, setFormData] = useState({
    template: "Hi there! You have {{ count }} new transactions in {{ account_name }}. Here's a summary:\n\n{{ transactions_table }}\n\nYou can {{ alter_settings }} your notification preferences at any time.",
    subject: "[YearlyReport] {{ count }} New Transactions in {{ account_name }}",
    numTransactions: 3,
    accountName: "Demo Account",
  });

  const [previewData, setPreviewData] = useState<{ html: string; subject: string } | null>(null);

  const previewMutation = useMutation({
    mutationFn: async () => {
      return NoCodeService.previewNotification({
        template: formData.template,
        subject: formData.subject,
        numTransactions: formData.numTransactions,
        accountName: formData.accountName
      });
    },
    onSuccess: (data) => {
      setPreviewData({
        html: data.html,
        subject: data.subject
      });
      showToast("Success!", "Preview generated successfully", "success");
    },
    onError: (error) => {
      showToast("Error", error.toString(), "error");
    },
  });

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    setFormData((prev) => ({ ...prev, [name]: value }));
  };

  const handleNumberChange = (name: string, value: number) => {
    setFormData((prev) => ({ ...prev, [name]: value }));
  };

  const handlePreview = () => {
    previewMutation.mutate();
  };

  return (
    <Box>
      <Card className="mb-6">
        <CardHeader>
          <Heading size="md">Test Notification Templates</Heading>
        </CardHeader>
        <CardContent>
          <Text mb={4}>
            Use this tool to preview how your notifications will look with sample data.
            You can customize the template and see the rendered result.
          </Text>

          <Stack gap={4} align="stretch">
            <FieldRoot>
              <FieldLabel htmlFor="subject">Email Subject</FieldLabel>
              <Input
                id="subject"
                name="subject"
                value={formData.subject}
                onChange={handleChange}
                placeholder="Enter email subject with template variables"
              />
            </FieldRoot>

            <FieldRoot>
              <FieldLabel htmlFor="template">Email Template</FieldLabel>
              <Textarea
                id="template"
                name="template"
                value={formData.template}
                onChange={handleChange}
                placeholder="Enter email template with variables"
                minHeight="150px"
              />
            </FieldRoot>

            <HStack gap={6}>
              <FieldRoot>
                <FieldLabel htmlFor="numTransactions">Number of Sample Transactions</FieldLabel>
                <Input
                  id="numTransactions"
                  name="numTransactions"
                  type="number"
                  min={1}
                  max={10}
                  value={formData.numTransactions}
                  onChange={(e) => handleNumberChange("numTransactions", parseInt(e.target.value))}
                />
              </FieldRoot>

              <FieldRoot>
                <FieldLabel htmlFor="accountName">Account Name</FieldLabel>
                <Input
                  id="accountName"
                  name="accountName"
                  value={formData.accountName}
                  onChange={handleChange}
                  placeholder="Enter account name"
                />
              </FieldRoot>
            </HStack>

            <Box>
              <Text fontSize="sm" color="gray.600" mb={2}>
                Available template variables: 
              </Text>
              <HStack gap={2}>
                <Badge>{'{{ count }}'}</Badge>
                <Badge>{'{{ account_name }}'}</Badge>
                <Badge>{'{{ transactions_table }}'}</Badge>
                <Badge>{'{{ alter_settings }}'}</Badge>
              </HStack>
            </Box>

            <Button
              colorScheme="blue"
              onClick={handlePreview}
              data-loading={previewMutation.isPending}
              alignSelf="flex-start"
              mt={4}
            >
              <Icon as={FaPlay} mr={2} />
              Generate Preview
            </Button>
          </Stack>
        </CardContent>
      </Card>

      {previewData && (
        <Card>
          <CardHeader>
            <Heading size="md">Preview Result</Heading>
          </CardHeader>
          <Separator className="mx-6" />
          <CardContent>
            <NotificationPreview 
              subject={previewData.subject} 
              html={previewData.html} 
            />
          </CardContent>
        </Card>
      )}
    </Box>
  );
}
