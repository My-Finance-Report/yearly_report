import { Email, NoCodeService } from "@/client";
import React, { useState } from "react";
import { Card } from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import { NotificationFormValues } from "./Builder";
import { useForm } from "react-hook-form";

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
    <Card.Root className="border" w="full">
      <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium">
        Preview
      </Card.Header>
      <Card.Body p={3}>
        <p>Preview not available</p>
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
    <Card.Root className="border" w="full" minW={"600px"}>
      <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium">
        {data.subject}
      </Card.Header>
      <Card.Body className="p-0 overflow-hidden">
        <div dangerouslySetInnerHTML={{ __html: data.clean_html }} />
      </Card.Body>
    </Card.Root>
  );
}
