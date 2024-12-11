"use client";

import styles from '../action.module.css';

import React from 'react';
import { useRouter } from 'next/navigation';

import ActionButton from '../action';

interface RedirectButtonProps extends React.HTMLAttributes<HTMLButtonElement> {
  icon: string;
  url: string;
  isActive: boolean;
}

/**
 * An action button that redirects to the target url.
 * 
 * @param {string} icon The Material icon name.
 * @param {string} url The redirect target url.
 * @param {boolean} isActive Indicates if the redirect button is active and should be highlighted.
 */
export default function RedirectButton({ icon, url, isActive, ...rest }: Readonly<RedirectButtonProps>) {
  const router = useRouter();
  const handleClick:React.MouseEventHandler<HTMLButtonElement> = (): void => {
    router.push(url);
  };

  return (
    <ActionButton
      icon={icon}
      className={`${isActive ? styles["active"] : ""}`}
      title={rest.title}
      onClick={handleClick}
    />
  );
}