import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/_visitor/$slug/")({
  component: RouteComponent,
});

function RouteComponent() {
  const { slug } = Route.useParams();
  return <div>Welcome to {slug}</div>;
}
