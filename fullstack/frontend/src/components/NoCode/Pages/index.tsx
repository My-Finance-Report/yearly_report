import { NoCodeService, PageVariant } from "@/client";
import { useQuery } from "@tanstack/react-query";
import { NoCodeDisplayCanvas } from "@/components/NoCode/Canvas";
import PageLoader from "@/components/Common/PageLoader";
import { NoCodeProvider } from "@/contexts/NoCodeContext";
import { useIsMobile } from "@/hooks/useIsMobile";

export function NoCodePage({ variant }: { variant: PageVariant }) {
  const isMobile = useIsMobile();
  const { data, isLoading, isError } = useQuery({
    queryKey: ["accounts-no-code", isMobile, variant],
    queryFn: () =>
      NoCodeService.getNoCodeDashboard({
        variant,
        screen: isMobile ? "mobile" : "desktop",
      }),
  });

  if (isLoading || !data) {
    return <PageLoader />;
  }

  if (isError) {
    throw new Error("Failed to load no code dashboard");
  }

  return (
    <NoCodeProvider parameters={data.parameters}>
      <NoCodeDisplayCanvas widgets={data.widgets} canvasId={data.canvas_id} />
    </NoCodeProvider>
  );
}
