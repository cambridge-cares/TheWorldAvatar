"use client"

import { useEffect, useState } from 'react';
import Image from 'next/image';

interface LandingImageProps {
  lightUrl: string;
  darkUrl: string;
}

/**
 * An image component that sets the landing page logo based on the current theme.
 * 
 * @prop {string} lightUrl The url for the light mode image.
 * @prop {string} darkUrl An optional url for the dark mode image. Defaults to light url if undefined
*/
export default function LandingImage(props: Readonly<LandingImageProps>) {
  const [imageUrl, setImageUrl] = useState<string>(props.lightUrl);

  useEffect(() => {
    const prefersDarkMode: boolean = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    // Only set dark mode image if the user has configured this
    if (typeof window !== "undefined" && prefersDarkMode && props.darkUrl) {
      setImageUrl(props.darkUrl);
    } else {
      setImageUrl(props.lightUrl);
    }
  }, []);

  return (
    <Image alt="Brand Logo with Text" src={imageUrl} width={200} height={200} />
  );
}