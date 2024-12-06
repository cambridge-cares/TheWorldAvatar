"use client";

import styles from './action.module.css';

import React from 'react';
import { Icon } from '@mui/material';

interface ActionButtonProps extends React.HTMLAttributes<HTMLButtonElement> {
  icon: string;
}

/**
 * A generic action button template class.
 * 
 * @param {string} icon The Material icon name.
 */
export default function ActionButton({ icon, ...rest }: Readonly<ActionButtonProps>) {
  return (
    <button className={`${rest.className} ${styles["button-container"]}`} onClick={rest.onClick}>
      <Icon className={`material-symbols-outlined ${styles["icon"]}`}>{icon}</Icon>
      <p className={styles["text"]}>{rest.title}</p>
    </button>
  );
}