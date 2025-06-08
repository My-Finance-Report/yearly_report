import { NoCodeService, PageVariant } from "@/client";
import { useQuery } from "@tanstack/react-query";
import { NoCodeDisplayCanvas } from "@/components/NoCode/Canvas";
import PageLoader from "@/components/Common/PageLoader";
import { NoCodeProvider } from "@/contexts/NoCodeContext";

export function NoCodePage({ variant }: { variant: PageVariant }) {
  const { data, isLoading, isError } = useQuery({
    queryKey: ["accounts-no-code"],
    queryFn: () => NoCodeService.getNoCodeDashboard({ variant }),
  });

  if (isLoading || !data) {
    return <PageLoader />;
  }

  if (isError) {
    throw new Error("Failed to load no code dashboard");
  }

  return (
    <NoCodeProvider parameters={data.parameters}>
      <NoCodeDisplayCanvas widgets={data.widgets} />
    </NoCodeProvider>
  );
}
