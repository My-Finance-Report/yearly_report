import { NoCodeWidgetOut, Parameter_Output } from "@/client";

export interface ShowProps {
    widget: NoCodeWidgetOut
    updateAParameter: (parameter: Parameter_Output, shouldRefetch: boolean) => void
}
