import { Box, TableRoot, TableHeader, TableRow, TableCell, TableBody, Heading } from "@chakra-ui/react"
import { NoCodeWidgetOut } from "@/client"

export function ShowList({ widget }: { widget: NoCodeWidgetOut }) {

    const result = widget.result as Array<{ [key: string]: string | number }>

    return (
        <Box>
        <Heading>{widget.name}</Heading>
        <TableRoot variant="outline" borderRadius="md" borderWidth={1}>
            <TableHeader>
                <TableRow>
                    {result.map((data, index) => {
                    if(index ===0){
                        return Object.keys(data).map((key) => (
                            <TableCell key={key}>{key}</TableCell>
                        ))
                    }
})}
                </TableRow>
            </TableHeader>
            <TableBody>
                {result.map((data,index) => (
                    <TableRow key={index}>
                        {Object.entries(data).map(([key, value]) => (
                            <TableCell key={key}>{value}</TableCell>
                        ))}
                    </TableRow>
                ))}
            </TableBody>
        </TableRoot>
</Box>
)
}

