import { Email, NoCodeService } from "@/client";
import React, { useState } from "react";
import { Box, Card, Skeleton, Text } from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { NotificationFormValues } from "./Builder";
import { useForm } from "react-hook-form";
import { FiMail } from "react-icons/fi";

interface NotificationPreviewProps {
  form: ReturnType<typeof useForm<NotificationFormValues>>;
}

export function NotificationPreview({ form }: NotificationPreviewProps) {
  const [formValues, setFormValues] = useState<NotificationFormValues>(
    form.getValues(),
  );

  form.watch((values) => {
    setFormValues(values as NotificationFormValues);
  });

  if (!formValues) {
    return <Placeholder />;
  }

  return <NotificationPreviewInner formValues={formValues} />;
}

function Placeholder() {
  return (
    <Card.Root w="full" variant="outline">
      <Card.Header p={4} borderBottomWidth="1px">
        <Text fontWeight="medium" color="gray.600">
          Preview
        </Text>
      </Card.Header>
      <Card.Body p={6}>
        <Box>
          <Skeleton height="24px" width="60%" mb={4} />
          <Skeleton height="16px" width="90%" mb={2} />
          <Skeleton height="16px" width="80%" mb={2} />
          <Skeleton height="16px" width="85%" />
        </Box>
      </Card.Body>
    </Card.Root>
  );
}

export function NotificationPreviewInner({
  formValues,
}: {
  formValues: NotificationFormValues;
}) {
  const [data, setData] = React.useState<Email | null>(null);
  const { data: queryData } = useQuery({
    queryKey: ["previewNotification", formValues.template, formValues.subject],
    queryFn: async () => {
      return NoCodeService.previewNotification({
        template: formValues.template,
        subject: formValues.subject,
      });
    },
    enabled: !!formValues.template && !!formValues.subject,
  });

  React.useEffect(() => {
    if (queryData) {
      setData(queryData);
    }
  }, [queryData]);

  if (!data) {
    return <Placeholder />;
  }

  return (
    <Card.Root
      w="full"
      minW={{ base: "full"}}
      maxW="900px"
      variant="outline"
      overflow="hidden"
    >
      <Card.Header p={4} borderBottomWidth="1px" display="flex" alignItems="center" gap={3}>
        <Box as={FiMail} color="blue.500" boxSize={5} />
        <Text fontWeight="medium" color="gray.700">
          {data.subject}
        </Text>
      </Card.Header>
      <Card.Body p={6}>
        <Box
          className="email-preview"
          p={6}
          borderRadius="md"
          boxShadow="sm"
        >
          <div dangerouslySetInnerHTML={{ __html: data.clean_html }} />
        </Box>
      </Card.Body>
    </Card.Root>
  );
}
