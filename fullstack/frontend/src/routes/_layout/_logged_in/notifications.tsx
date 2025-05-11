import { createFileRoute } from "@tanstack/react-router";
import { NoCodeService } from "@/client";
import React, { useState, useRef, useEffect } from "react";
import {
  Box,
  Button,
  Heading,
  HStack,
  Input,
  Spinner,
  FieldRoot,
  FieldLabel,
  Text,
  Textarea,
  Card,
  VStack,
} from "@chakra-ui/react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { EffectOut } from "@/client/types.gen";
import { NoCodeUpdateEffectData } from "@/client/types.gen";
import useCustomToast from "@/hooks/useCustomToast";

export const Route = createFileRoute("/_layout/_logged_in/notifications")({
  component: NotificationsPage,
});

function NotificationsPage() {
  return (
    <Box p={4}>
      <Heading mb={4}>Notifications</Heading>
      <UnifiedNotificationInterface />
    </Box>
  );
}

function UnifiedNotificationInterface() {
  const [previewData, setPreviewData] = useState<{ html: string; subject: string } | null>(null);
  const [selectedEffect, setSelectedEffect] = useState<EffectOut | null>(null);
  const [isEditing, setIsEditing] = useState(false);
  const [formData, setFormData] = useState({
    name: "",
    template: "Hi there! You have {{ count }} new transactions in {{ account_name }}. Here's a summary:\n\n{{ transactions_table }}\n\nYou can {{ alter_settings }} your notification preferences at any time.",
    subject: "[YearlyReport] {{ count }} New Transactions in {{ account_name }}",
    numTransactions: 3,
    accountName: "Demo Account",
  });

  const showToast = useCustomToast();
  const queryClient = useQueryClient();

  // Fetch all notification effects
  const { data: effects, isLoading } = useQuery({
    queryKey: ["effects"],
    queryFn: () => NoCodeService.getEffects(),
  });

  // Preview mutation
  const previewMutation = useMutation({
    mutationFn: async () => {
      return NoCodeService.previewNotification({
        template: formData.template,
        subject: formData.subject,
        numTransactions: formData.numTransactions,
        accountName: formData.accountName,
      });
    },
    onSuccess: (data) => {
      setPreviewData({
        html: data.html,
        subject: data.subject,
      });
      showToast("Preview generated", "Preview generated successfully.", "success");
    },
    onError: (error) => {
      showToast("Error generating preview", error.message, "error");
    },
  });

  // Create mutation
  const createMutation = useMutation({
    mutationFn: async () => {
      return NoCodeService.createEffect({
        effect_type: "email",
        event_type: "new_transaction",
        name: formData.name,
        frequency_days: 1,
        template: formData.template,
        subject: formData.subject,
        condition: "count_of_transactions",
        conditional_parameters: { count: 1 },
      });
    },
    onSuccess: () => {
      showToast("Notification saved", "Notification saved successfully.", "success");
      queryClient.invalidateQueries({ queryKey: ["effects"] });
      setFormData({
        ...formData,
        name: "",
      });
    },
    onError: (error) => {
      showToast("Error saving notification", error.message, "error");
    },
  });

  // Update mutation
  const updateMutation = useMutation({
    mutationFn: (data: { id: number; effect: NoCodeUpdateEffectData }) => {
      return NoCodeService.updateEffect({ id: data.id, ...data.effect });
    },
    onSuccess: () => {
      showToast("Notification updated", "Notification updated successfully.", "success");
      setIsEditing(false);
      setSelectedEffect(null);
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast("Error updating notification", error.message, "error");
    },
  });

  // Delete mutation
  const deleteMutation = useMutation({
    mutationFn: (id: number) => NoCodeService.deleteEffect({ id }),
    onSuccess: () => {
      showToast("Notification deleted", "Notification deleted successfully.", "success");
      queryClient.invalidateQueries({ queryKey: ["effects"] });
    },
    onError: (error) => {
      showToast("Error deleting notification", error.message, "error");
    },
  });

  const handleEdit = (effect: any) => {
    setSelectedEffect(effect);
    setFormData({
      name: effect.name,
      template: effect.config.template,
      subject: effect.config.subject,
      numTransactions: 3,
      accountName: "Demo Account",
    });
    setIsEditing(true);
    // Generate a preview immediately
    previewMutation.mutate();
  };

  const handleUpdate = () => {
    if (!selectedEffect || !isEditing) return;

    if (selectedEffect.id) {
      updateMutation.mutate({
        id: selectedEffect.id,
        effect: {
          name: formData.name,
          effect_type: selectedEffect.effect_type,
          event_type: selectedEffect.event_type,
          frequency_days: selectedEffect.config.frequency_days,
          template: formData.template,
          subject: formData.subject,
          condition: selectedEffect.condition,
          conditional_parameters: selectedEffect.conditional_parameters,
        },
      });
    }
  };

  const handleDelete = (id: number) => {
    // Check if the user has chosen to skip confirmation dialogs for this entity type
    const skipConfirmation = localStorage.getItem("skipDeleteConfirmation_notification") === "true";

    if (skipConfirmation || confirm("Are you sure you want to delete this notification?")) {
      deleteMutation.mutate(id);
    }
  };

  const handleSave = () => {
    if (isEditing) {
      handleUpdate();
    } else {
      // Check if name is provided
      if (!formData.name) {
        const name = prompt("Enter a name for this notification:");
        if (name) {
          setFormData({ ...formData, name });
          createMutation.mutate();
        }
      } else {
        createMutation.mutate();
      }
    }
  };

  const handleReset = () => {
    setFormData({
      name: "",
      template: "Hi there! You have {{ count }} new transactions in {{ account_name }}. Here's a summary:\n\n{{ transactions_table }}\n\nYou can {{ alter_settings }} your notification preferences at any time.",
      subject: "[YearlyReport] {{ count }} New Transactions in {{ account_name }}",
      numTransactions: 3,
      accountName: "Demo Account",
    });
    setSelectedEffect(null);
    setIsEditing(false);
    setPreviewData(null);
  };

  if (isLoading) {
    return <Spinner />;
  }

  return (
    <VStack spaceY={6} align="stretch">
      {/* Template Editor Section */}
      <Card.Root>
        <Card.Header>
          <HStack justifyContent="space-between">
            <Heading size="md">{isEditing ? "Edit Notification" : "Create Notification"}</Heading>
            {isEditing && (
              <Button size="sm" onClick={handleReset}>
                Cancel Editing
              </Button>
            )}
          </HStack>
        </Card.Header>
        <Card.Body>
          <VStack spaceY={4} align="stretch">
            <FieldRoot>
              <FieldLabel>Name</FieldLabel>
              <Input
                value={formData.name}
                onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                placeholder="Enter a name for this notification"
              />
            </FieldRoot>

            <FieldRoot>
              <FieldLabel>Template</FieldLabel>
              <Textarea
                value={formData.template}
                onChange={(e) => setFormData({ ...formData, template: e.target.value })}
                rows={6}
              />
            </FieldRoot>

            <FieldRoot>
              <FieldLabel>Subject</FieldLabel>
              <Input
                value={formData.subject}
                onChange={(e) => setFormData({ ...formData, subject: e.target.value })}
              />
            </FieldRoot>

            <HStack spaceX={4}>
              <FieldRoot>
                <FieldLabel>Number of Transactions</FieldLabel>
                <Input
                  type="number"
                  value={formData.numTransactions}
                  onChange={(e) => setFormData({ ...formData, numTransactions: Number(e.target.value) })}
                  min={1}
                  max={10}
                />
              </FieldRoot>

              <FieldRoot>
                <FieldLabel>Account Name</FieldLabel>
                <Input
                  value={formData.accountName}
                  onChange={(e) => setFormData({ ...formData, accountName: e.target.value })}
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
                  loading={isEditing ? updateMutation.isPending : createMutation.isPending}
                  onClick={handleSave}
                >
                  {isEditing ? "Update Notification" : "Save as Notification"}
                </Button>
              )}
            </HStack>
          </VStack>
        </Card.Body>
      </Card.Root>

      {/* Preview Section */}
      {previewData && (
        <Card.Root>
          <Card.Header>
            <Heading size="md">Preview</Heading>
          </Card.Header>
          <Card.Body>
            <NotificationPreview
              subject={previewData.subject}
              html={previewData.html}
            />
          </Card.Body>
        </Card.Root>
      )}

      {/* Existing Notifications Section */}
      <Card.Root>
        <Card.Header>
          <Heading size="md">Your Notifications</Heading>
        </Card.Header>
        <Card.Body>
          {effects?.length === 0 ? (
            <Text textAlign="center" py={4}>
              No notifications found. Create one using the form above.
            </Text>
          ) : (
            <VStack spaceY={4} align="stretch">
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
                          onClick={() => effect.id && handleDelete(effect.id)}
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

              {/* Don't show me again checkbox for delete confirmations */}
              <HStack>
                <input
                  type="checkbox"
                  id="skipDeleteConfirmation"
                  onChange={(e) => {
                    localStorage.setItem("skipDeleteConfirmation_notification", e.target.checked.toString());
                  }}
                  checked={localStorage.getItem("skipDeleteConfirmation_notification") === "true"}
                />
                <label htmlFor="skipDeleteConfirmation">
                  Don't ask for confirmation when deleting notifications
                </label>
              </HStack>
            </VStack>
          )}
        </Card.Body>
      </Card.Root>
    </VStack>
  );
}

interface NotificationPreviewProps {
  html: string;
  subject: string;
}

function NotificationPreview({ html, subject }: NotificationPreviewProps) {
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
