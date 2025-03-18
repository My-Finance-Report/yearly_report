import { createFileRoute } from '@tanstack/react-router'

export const Route = createFileRoute('/terms')({
  component: Terms
})

function Terms() {

    return (
<div>Hello /terms!
</div>)
}
