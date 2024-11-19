"use client"

import { useEffect, useState } from 'react';

import { Assets } from 'io/config/assets';

/**
 * An image component that sets the background image depending on the current theme.
  */

const ASSET_PREFIX = process.env.ASSET_PREFIX;

export default function BackgroundImage() {
  const lightBackgroundUrl: string = ASSET_PREFIX + Assets.BACKGROUND_LIGHT;
  const darkBackgroundUrl: string = ASSET_PREFIX + Assets.BACKGROUND_DARK;
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