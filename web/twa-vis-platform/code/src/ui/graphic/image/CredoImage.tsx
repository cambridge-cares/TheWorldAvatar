"use client"

import { useEffect, useState } from 'react';
import Image from 'next/image';

/**
 * An image component that sets the background image depending on the current theme.
  */
export default function CredoImage() {
  const lightURL: string = "./images/credo-misc/credo-light.svg";
  const darkURL: string = "./images/credo-misc/credo-dark.svg";
  const [ImageURL, setImageUrl] = useState(lightURL);

  useEffect(() => {
    const prefersDarkMode: boolean = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    if (typeof window !== "undefined" && prefersDarkMode) {
      setImageUrl(darkURL);
    } else {
      setImageUrl(lightURL);
    }
  }, []);

  return (
    <Image alt="CReDo Logo with Text" src={ImageURL} width={350} height={350} />
  );
}