"use client"

import { useEffect, useState } from 'react';

/**
 * An image component that sets the background image depending on the current theme.
  */
export default function BackgroundImage() {
  const lightBackgroundUrl: string = "/images/defaults/background-light.svg";
  const darkBackgroundUrl: string = "/images/defaults/background-dark.svg";
  const [backgroundImageUrl, setBackgroundImageUrl] = useState(lightBackgroundUrl);

  useEffect(() => {
    const prefersDarkMode: boolean = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    if (typeof window !== "undefined" && prefersDarkMode) {
      setBackgroundImageUrl(darkBackgroundUrl);
    } else {
      setBackgroundImageUrl(lightBackgroundUrl);
    }
  }, []);

  return (
    // eslint-disable-next-line
    <style jsx global>{`
      body {
        background-image: url('${backgroundImageUrl}');
      }
    `}</style>
  );
}