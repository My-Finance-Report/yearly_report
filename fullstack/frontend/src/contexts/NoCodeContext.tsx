import React, {
  type ReactNode,
  createContext,
  useContext,
  useEffect,
  useState,
} from "react";
import { Parameter_Output } from "@/client";

interface NoCodeContextType {
  parameters: Parameter_Output[];
  setParameters: React.Dispatch<React.SetStateAction<Parameter_Output[]>>;
  updateParameter: (param: Parameter_Output) => void;
  widgetParameters: Record<
    string,
    React.Dispatch<React.SetStateAction<Parameter_Output[]>>
  >;
  setWidgetParameters: React.Dispatch<
    React.SetStateAction<
      Record<string, React.Dispatch<React.SetStateAction<Parameter_Output[]>>>
    >
  >;
  getParamsForView: (view: string) => Parameter_Output[];
}

const NoCodeContext = createContext<NoCodeContextType | undefined>(undefined);

export function NoCodeProvider({
  children,
  parameters: initParameters,
}: {
  children: ReactNode;
  parameters: Parameter_Output[];
}) {
  const [parameters, setParameters] =
    useState<Parameter_Output[]>(initParameters);
  const [widgetParameters, setWidgetParameters] = useState<
    Record<string, React.Dispatch<React.SetStateAction<Parameter_Output[]>>>
  >({});

  useEffect(() => {
    setParameters(initParameters);
  }, [initParameters]);

  const getParamsForView = (view: string) =>
    parameters.filter(
      (parameter) =>
        parameter.display_info && parameter.display_info.views.includes(view),
    );

  const updateParameter = (parameter: Parameter_Output) => {
    const newParams = parameters.map((p) =>
      p.id === parameter.id ? parameter : p,
    );

    setParameters(newParams);

    if (parameter.trigger_refetch) {
      parameter.dependent_widgets?.map((widget_id) => {
        const callable = widgetParameters[widget_id];

        callable?.((prev) =>
          prev.map((p) => (p.id === parameter.id ? parameter : p)),
        );
      });
    }
  };

  const contextValue: NoCodeContextType = {
    parameters,
    setParameters,
    updateParameter,
    setWidgetParameters,
    widgetParameters,
    getParamsForView,
  };

  return (
    <NoCodeContext.Provider value={contextValue}>
      {children}
    </NoCodeContext.Provider>
  );
}

export function useNoCodeContext() {
  const context = useContext(NoCodeContext);

  if (context === undefined) {
    throw new Error("useNoCodes must be used within a NoCodeProvider");
  }

  return context;
}
