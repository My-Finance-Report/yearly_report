import { NoCodeService, PageVariant } from "@/client";
import { useQuery } from "@tanstack/react-query";
import { NoCodeDisplayCanvas } from "@/components/NoCode/Canvas";
import { Spinner } from "@chakra-ui/react";
import { NoCodeProvider } from "@/contexts/NoCodeContext";

export function NoCodePage({ variant }: { variant: PageVariant }) {
  const { data, isLoading, isError } = useQuery({
    queryKey: ["accounts-no-code"],
    queryFn: () => NoCodeService.getNoCodeDashboard({ variant }),
  });

  if (isLoading || !data) {
    return <Spinner />;
  }

  if (isError) {
    return <h1>there has been an error</h1>;
  }

  return (
    <NoCodeProvider parameters={data.parameters}>
      <NoCodeDisplayCanvas widgets={data.widgets} canvasId={data.canvas_id} />
    </NoCodeProvider>
  );
}
