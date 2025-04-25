import { useEffect, useState } from "react";

function useMediaQuery(predicate: (width: number) => boolean) {
  const [matches, setMatches] = useState(false);

  useEffect(() => {
    if (typeof window === "undefined") return;

    const update = () => setMatches(predicate(window.outerWidth));

    update();
    window.addEventListener("resize", update);
    return () => window.removeEventListener("resize", update);
  }, [predicate, window]);

  return matches;
}

export function useIsMobile(breakpoint = 768) {
  return useMediaQuery((width) => width < breakpoint);
}

export function useIsDesktop(breakpoint = 1260) {
  return useMediaQuery((width) => width >= breakpoint);
}
