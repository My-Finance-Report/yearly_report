import { Button } from "@chakra-ui/react"
import { useState } from "react"
import { UploadDialog } from "@/components/Common/OnboardModal/Onboarding"

export function UploadButton(){

    const [isUploadOpen, setIsUploadOpen] = useState(false)
    return (
        <>
            <Button size="sm" onClick={() => setIsUploadOpen(true)}>Upload Files</Button>
            <UploadDialog isUploadOpen={isUploadOpen} onUploadClose={() => setIsUploadOpen(false)} />
        </>
    )
}