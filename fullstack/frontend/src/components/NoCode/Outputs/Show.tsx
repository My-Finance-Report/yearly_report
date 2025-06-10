import {
  NoCodeService,
  NoCodeWidgetIn_Output,
  Parameter_Output,
} from "@/client";
import { NoCodeParameter } from "@/components/NoCode/Generators/Parameter";
import {
  ShowValue,
  ShowBadge,
  ShowValueWithTrend,
  ShowSeparator,
} from "./ShowValue";
import { ShowList } from "./ShowList";
import { ShowForm } from "./ShowForm";
import { ShowPieChart } from "./ShowPieChart";
import { ShowBarChart } from "./ShowBarChart";
import { useQuery } from "@tanstack/react-query";
import { Spinner, Text } from "@chakra-ui/react";

import { useNoCodeContext } from "@/contexts/NoCodeContext";

const MAP_TO_SHOW = {
  value: ShowValue,
  value_with_trend: ShowValueWithTrend,
  list: ShowList,
  form: ShowForm,
  pie_chart: ShowPieChart,
  bar_chart: ShowBarChart,
  badge: ShowBadge,
  separator: ShowSeparator,
};

export function renderNoCodeParameter(
  parameter: Parameter_Output,
  closeModal?: () => void,
) {
  const { updateParameter } = useNoCodeContext();

  switch (parameter.type) {
    case "int":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "int" }>}
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "float":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "float" }>}
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "string":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "string" }>}
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "select":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "select" }>}
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "multi_select":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={
            parameter as Extract<Parameter_Output, { type: "multi_select" }>
          }
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "pagination":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={
            parameter as Extract<Parameter_Output, { type: "pagination" }>
          }
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );
    case "datetime":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "submit" }>}
          onChange={(value) => updateParameter({ ...parameter, value })}
        />
      );

    case "submit":
      return (
        <NoCodeParameter
          key={parameter.name}
          parameter={parameter as Extract<Parameter_Output, { type: "submit" }>}
          onChange={(value) => {
            updateParameter({ ...parameter, value });
            closeModal?.();
          }}
        />
      );
    default:
      throw new Error("unknown param");
  }
}

export function NoCodeWidget({ widget }: { widget: NoCodeWidgetIn_Output }) {
  const TheDisplay = MAP_TO_SHOW[widget.type];

  const { parameters } = useNoCodeContext();

  const { data, isLoading, isFetching, isError } = useQuery({
    queryKey: ["accounts-no-code", widget.id],
    queryFn: () =>
      NoCodeService.refetchWidget({
        widgetId: widget.id,
        requestBody: parameters,
      }),
  });

  if (isError) {
    return <Text>unable to fetch</Text>;
  }

  if (isLoading || isFetching || !data) {
    return <Spinner />;
  }

  return <TheDisplay widget={data} />;
}
