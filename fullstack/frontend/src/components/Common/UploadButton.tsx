import { Button } from "@chakra-ui/react";
import { useState } from "react";
import { UploadDialog } from "@/components/Common/OnboardModal/Onboarding";
import { FaPlus } from "react-icons/fa";

export function UploadButton() {
  const [isUploadOpen, setIsUploadOpen] = useState(false);
  return (
    <>
      <Button size="sm" onClick={() => setIsUploadOpen(true)}>
        <FaPlus />
        Upload Files
      </Button>
      <UploadDialog
        isUploadOpen={isUploadOpen}
        onUploadClose={() => setIsUploadOpen(false)}
      />
    </>
  );
}
