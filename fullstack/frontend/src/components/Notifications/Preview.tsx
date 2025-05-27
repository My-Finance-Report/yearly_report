import { NoCodeService } from "@/client";
import { useState, useEffect } from "react";
import {
  Card,
} from "@chakra-ui/react";
import { useQuery } from "@tanstack/react-query";
import {
  Email,
} from "@/client/types.gen";
import { NotificationFormValues } from "./Builder";



interface NotificationPreviewProps {
  formValues: NotificationFormValues | null;
}



export function NotificationPreview({ formValues }: NotificationPreviewProps) {
  if (!formValues) {
    return <Placeholder />;
  }

  return <NotificationPreviewInner formValues={formValues} />;
}

function Placeholder() {
  return <Card.Root className="border" w="full">
    <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium">
      Preview
    </Card.Header>
    <Card.Body p={3}>
      <p>Preview not available</p>
    </Card.Body>
  </Card.Root>;
}

export function NotificationPreviewInner({ formValues }: { formValues: NotificationFormValues }) {
  const [previewData, setPreviewData] = useState<Email | null>(null);
  const { data, refetch } = useQuery({
    queryKey: ["previewNotification", formValues.template, formValues.subject],
    queryFn: async () => {
      return NoCodeService.previewNotification({
        template: formValues.template,
        subject: formValues.subject,
        numTransactions: 3,
        accountName: "Test Account",
      });
    },
    enabled: !!formValues.template && !!formValues.subject,
  });

  useEffect(() => {
    refetch();
    if (!data) return;
    setPreviewData(data);
  }, [formValues]);

  if (!previewData) {
    return <Placeholder />;
  }

  return (
    <Card.Root className="border" w="full">
      <Card.Header p={3} borderBottomWidth="1px" fontWeight="medium">
        {previewData.subject}
      </Card.Header>
      <Card.Body className="p-0 overflow-hidden">
        <div dangerouslySetInnerHTML={{ __html: previewData.html }} />
      </Card.Body>
    </Card.Root>
  );
}
