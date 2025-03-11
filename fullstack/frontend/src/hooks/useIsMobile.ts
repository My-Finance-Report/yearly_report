import { useState, useEffect } from "react"

export function useIsMobile(breakpoint = 768) {
  const [isMobile, setIsMobile] = useState(false)

  useEffect(() => {
    if (typeof window === "undefined") return

    const mediaQueryList = window.matchMedia(`(max-width: ${breakpoint}px)`)

    const updateIsMobile = (event: MediaQueryListEvent | MediaQueryList) => {
      setIsMobile(event.matches)
    }


    updateIsMobile(mediaQueryList)

    mediaQueryList.addEventListener("change", updateIsMobile)
    return () => {
      mediaQueryList.removeEventListener("change", updateIsMobile)
    }
  }, [breakpoint])

  return isMobile
}
