import React from 'react';
import Link from 'next/link';

import { formatAppUrl } from 'utils/client-utils';

interface AppLinkProps {
  url: string;
  className?: string;
  children: React.ReactNode;
  onClick?: () => void;
}

/**
 * A custom link component that adapts to any reverse proxy if set within this app.
 * 
 * @param {string} url - The relative url within the app. Do not add / at the start, as this has been added.
 * @param {string} className - Additional CSS classes to apply to the link.
 * @param {React.ReactNode} children - The content of the link (usually text or other React components).
 * @returns A link element with the prefixed URL.
 */
export default function AppLink(props: Readonly<AppLinkProps>) {
  const url: string = formatAppUrl(props.url);
  return (
    <Link href={url} className={props.className} onClick={props.onClick}>
      {props.children}
    </Link>
  )
}