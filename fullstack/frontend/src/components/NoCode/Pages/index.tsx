import { NoCodeService, PageVariant, Parameter_Output } from "@/client"
import { useQuery } from "@tanstack/react-query"
import {NoCodeDisplayCanvas} from "@/components/NoCode/Canvas"
import { Spinner } from "@chakra-ui/react"
import { useEffect, useState } from "react"

export function NoCodePage({variant}: {variant: PageVariant}){

    const [parameters, setParameters] = useState<Parameter_Output[]>([])

    const {
        data,
        isLoading,
        isError,
        refetch
    } = useQuery({
        queryKey: ["accounts-no-code"],
        queryFn: () => NoCodeService.getNoCodeDashboard({variant, requestBody: parameters}),
    })

    useEffect(() => {
        refetch()
    }, [parameters])

    if (isLoading || !data){
        return (<Spinner/>)
    }

    if (isError){
        return (<h1>there has been an error</h1>)
    }

    return <NoCodeDisplayCanvas widgets={data.widgets} globalParameters={data.global_parameters} setParameters={setParameters}/>

}