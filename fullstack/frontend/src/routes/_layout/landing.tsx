import { createFileRoute, redirect } from "@tanstack/react-router";

export const Route = createFileRoute("/_layout/landing")({
  component: Landing,
  beforeLoad: async () => {
    throw redirect({
      to: "/",
    });
  },
});

export default function Landing() {
  return null
}
