import { createFileRoute } from "@tanstack/react-router";
import { EffectOut, NoCodeService } from "@/client";
import {
  Text,
  Flex,
  Input,
  Box,
  Heading,
  VStack,
  HStack,
  Tabs,
  Card,
  Button,
  Spinner,
  FieldRoot,
  FieldLabel,
  Textarea,
  Icon,
  Badge,
  Tooltip,
  Separator,
} from "@chakra-ui/react";
import useCustomToast from "@/hooks/useCustomToast";
import { useRef, useEffect, useState } from "react";
import { FaBell, FaEnvelope, FaEdit, FaToggleOn, FaPlay, FaSave } from "react-icons/fa";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";

export const Route = createFileRoute("/_layout/_logged_in/notifications")({
  component: NotificationsPage,
});

function NotificationsPage() {
  return (
    <Box p={4}>
      <Heading mb={4}>Notifications</Heading>
      <Tabs.Root>
        <Tabs.List>
          <Tabs.Trigger value="settings">Notification Settings</Tabs.Trigger>
          <Tabs.Trigger value="tester">Test Notifications</Tabs.Trigger>
          <Tabs.Trigger value="manager">Manage Notifications</Tabs.Trigger>
        </Tabs.List>
        <Tabs.Content value="settings">
          <NotificationSettings />
        </Tabs.Content>
        <Tabs.Content value="tester">
          <NotificationTester />
        </Tabs.Content>
        <Tabs.Content value="manager">
          <NotificationManager />
        </Tabs.Content>
      </Tabs.Root>
    </Box>
  );
}

function NotificationSettings() {
  const { data: effects, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  if (isLoading) {
    return <Spinner />;
  }

  return (
    <VStack spaceX={4} align="stretch">
      <Text>Configure your notification settings below:</Text>
      {effects?.map((effect) => (
        <NotificationEffect key={effect.id || effect.name} effect={effect} />
      ))}
    </VStack>
  );
}

function NotificationEffect({ effect }: { effect: EffectOut }) {
  return (
    <Card.Root className="overflow-hidden shadow-md">
      <Flex p={4} borderBottomWidth="1px" align="center">
        <Icon
          as={effect.effect_type === "email" ? FaEnvelope : FaBell}
          color={effect.effect_type === "email" ? "blue.500" : "purple.500"}
          mr={3}
          fontSize="xl"
        />
        <Heading size="md">{effect.name}</Heading>
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

      <Card.Body>
        <VStack gap={4} align="stretch">
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
        </VStack>
      </Card.Body>

      <Card.Footer className="justify-end border-t">
        <Button
          size="sm"
          colorScheme="red"
          variant="outline"
        >
          <Icon as={FaEdit} mr={2} />
          Delete
        </Button>
      </Card.Footer>
    </Card.Root>
  );
}

function NotificationCondition({ effect }: { effect: EffectOut }) {
  return getStatement(effect);
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
    <Card.Root className="border">
      <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium" bg="gray.50">
        {subject}
      </Card.Header>
      {html ? (
        <Card.Body className="p-0 overflow-hidden">
          <iframe 
            ref={iframeRef} 
            title="Email Preview" 
            className="w-full border-0 min-h-[300px]" 
            sandbox="allow-same-origin"
          />
        </Card.Body>
      ) : (
        <Card.Body>{template}</Card.Body>
      )}
    </Card.Root>
  );
}

function NotificationManager() {
  const showToast = useCustomToast();
  const queryClient = useQueryClient();
  const [isEditing, setIsEditing] = useState(false);
  const [currentEffect, setCurrentEffect] = useState<any>(null);
  
  const { data: effects, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  const deleteMutation = useMutation({
    mutationFn: (id: number) => NoCodeService.deleteEffect(id),
    onSuccess: () => {
      showToast(
        "Notification deleted",
        "Notification deleted successfully.",
        "success"
      );
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast(
        "Error deleting notification",
        error.message,
        "error"
      );
    },
  });

  const updateMutation = useMutation({
    mutationFn: (data: { id: number; effect: any }) => {
      return NoCodeService.updateEffect(data.id, data.effect);
    },
    onSuccess: () => {
      showToast(
        "Notification updated",
        "Notification updated successfully.",
        "success"
      );
      setIsEditing(false);
      setCurrentEffect(null);
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast(
        "Error updating notification",
        error.message,
        "error"
      );
    },
  });

  const handleEdit = (effect: any) => {
    setCurrentEffect({
      id: effect.id,
      name: effect.name,
      effectType: effect.effect_type,
      eventType: effect.event_type,
      frequencyDays: effect.config.frequency_days,
      template: effect.config.template,
      subject: effect.config.subject,
      condition: effect.condition,
      conditionalParameters: effect.conditional_parameters,
    });
    setIsEditing(true);
  };

  const handleUpdate = () => {
    if (!currentEffect) return;
    
    updateMutation.mutate({
      id: currentEffect.id,
      effect: {
        name: currentEffect.name,
        effectType: currentEffect.effectType,
        eventType: currentEffect.eventType,
        frequencyDays: currentEffect.frequencyDays,
        template: currentEffect.template,
        subject: currentEffect.subject,
        condition: currentEffect.condition,
        conditionalParameters: currentEffect.conditionalParameters,
      },
    });
  };

  const handleDelete = (id: number) => {
    if (confirm("Are you sure you want to delete this notification?")) {
      deleteMutation.mutate(id);
    }
  };

  if (isLoading) {
    return <Spinner />;
  }

  if (isEditing && currentEffect) {
    return (
      <VStack spaceY={4} align="stretch">
        <HStack justifyContent="space-between">
          <Heading size="md">Edit Notification</Heading>
          <Button onClick={() => setIsEditing(false)}>Cancel</Button>
        </HStack>
        
        <Card.Root>
          <Card.Body>
            <VStack spaceY={4} align="stretch">
              <FieldRoot>
                <FieldLabel>Name</FieldLabel>
                <Input
                  value={currentEffect.name}
                  onChange={(e) => setCurrentEffect({ ...currentEffect, name: e.target.value })}
                />
              </FieldRoot>
              
              <FieldRoot>
                <FieldLabel>Template</FieldLabel>
                <Textarea
                  value={currentEffect.template}
                  onChange={(e) => setCurrentEffect({ ...currentEffect, template: e.target.value })}
                  rows={6}
                />
              </FieldRoot>

              <FieldRoot>
                <FieldLabel>Subject</FieldLabel>
                <Input
                  value={currentEffect.subject}
                  onChange={(e) => setCurrentEffect({ ...currentEffect, subject: e.target.value })}
                />
              </FieldRoot>

              <FieldRoot>
                <FieldLabel>Frequency (days)</FieldLabel>
                <Input
                  type="number"
                  value={currentEffect.frequencyDays}
                  onChange={(e) => setCurrentEffect({ ...currentEffect, frequencyDays: Number(e.target.value) })}
                  min={1}
                  max={30}
                />
              </FieldRoot>

              <Button
                colorScheme="blue"
                loading={updateMutation.isPending}
                onClick={handleUpdate}
              >
                Save Changes
              </Button>
            </VStack>
          </Card.Body>
        </Card.Root>
      </VStack>
    );
  }

  return (
    <VStack spaceY={4} align="stretch">
      <Text>Manage your notification templates:</Text>
      
      {effects?.map((effect) => (
        <Card.Root key={effect.id || effect.name} className="border">
          <Card.Header>
            <HStack justifyContent="space-between">
              <Heading size="md">{effect.name}</Heading>
              <HStack>
                <Button
                  size="sm"
                  variant="ghost"
                  onClick={() => handleEdit(effect)}
                >
                  Edit
                </Button>
                <Button
                  size="sm"
                  variant="ghost"
                  colorScheme="red"
                  onClick={() => handleDelete(effect.id!)}
                >
                  Delete
                </Button>
              </HStack>
            </HStack>
          </Card.Header>
          <Card.Body>
            <VStack align="start" spaceY={2}>
              <Text fontWeight="bold">Subject:</Text>
              <Text>{effect.config.subject}</Text>
              <Text fontWeight="bold">Template:</Text>
              <Text whiteSpace="pre-wrap">{effect.config.template}</Text>
              <Text fontWeight="bold">Frequency:</Text>
              <Text>Every {effect.config.frequency_days} day(s)</Text>
              <Text fontWeight="bold">Condition:</Text>
              <Text>{effect.condition}</Text>
            </VStack>
          </Card.Body>
        </Card.Root>
      ))}

      {effects?.length === 0 && (
        <Card.Root className="border">
          <Card.Body>
            <Text textAlign="center" py={4}>
              No notifications found. Create one using the Test Notifications tab.
            </Text>
          </Card.Body>
        </Card.Root>
      )}
    </VStack>
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
      showToast(
        "Preview generated",
        "Preview generated successfully.",
        "success",
      );
    },
    onError: (error) => {
      showToast(
        "Error generating preview",
        error.message,
        "error",
      );
    },
  });

  const queryClient = useQueryClient();

  const saveAsMutation = useMutation({
    mutationFn: async () => {
      return NoCodeService.createEffect({
        name: formData.name,
        effectType: "email",
        eventType: "new_transaction",
        frequencyDays: 1,
        template: formData.template,
        subject: formData.subject,
        condition: "count_of_transactions",
        conditionalParameters: { count: 1 }
      });
    },
    onSuccess: () => {
      showToast(
        "Notification saved",
        "Notification saved successfully.",
        "success",
      );
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast(
        "Error saving notification",
        error.message,
        "error",
      );
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
    <VStack spaceY={4} align="stretch">
      <Text>Test your notification templates here:</Text>

      <Card.Root>
        <Card.Body>
          <VStack spaceY={4} align="stretch">
            <FieldRoot>
              <FieldLabel>Template</FieldLabel>
              <Textarea
                value={formData.template}
                onChange={(e) =>
                  setFormData({ ...formData, template: e.target.value })
                }
                rows={6}
              />
            </FieldRoot>

            <FieldRoot>
              <FieldLabel>Subject</FieldLabel>
              <Input
                value={formData.subject}
                onChange={(e) =>
                  setFormData({ ...formData, subject: e.target.value })
                }
              />
            </FieldRoot>

            <HStack spaceX={4}>
              <FieldRoot>
                <FieldLabel>Number of Transactions</FieldLabel>
                <Input
                  value={formData.numTransactions}
                  type="number"
                  onChange={(e) =>
                    setFormData({ ...formData, numTransactions: Number(e.target.value) })
                  }
                  min={1}
                  max={10}
                />
              </FieldRoot>

              <FieldRoot>
                <FieldLabel>Account Name</FieldLabel>
                <Input
                  value={formData.accountName}
                  onChange={(e) =>
                    setFormData({ ...formData, accountName: e.target.value })
                  }
                />
              </FieldRoot>
            </HStack>

            <HStack>
              <Button
                colorScheme="blue"
                loading={previewMutation.isPending}
                onClick={() => previewMutation.mutate()}
                flex="1"
              >
                Generate Preview
              </Button>
              
              {previewData && (
                <Button
                  colorScheme="green"
                  loading={saveAsMutation.isPending}
                  onClick={() => {
                    const name = prompt("Enter a name for this notification:");
                    if (name) {
                      saveAsMutation.mutate(name);
                    }
                  }}
                >
                  Save as Notification
                </Button>
              )}
            </HStack>
          </VStack>
        </Card.Body>
      </Card.Root>

      {previewData && (
        <Box>
          <Heading size="md" mb={2}>
            Preview:
          </Heading>
          <NotificationPreview
            subject={previewData.subject}
            html={previewData.html}
          />
        </Box>
      )}
    </VStack>
  );
}
