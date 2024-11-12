"use client";

import styles from './navbar-component.module.css';

import React from 'react';
import Icon from '@mui/material/Icon';
import { Tooltip } from '@mui/material';
import Link from 'next/link';


// Type definition for incoming parameters
export interface NavbarComponentProps {
  name: string,
  tooltip: string,
  icon: string,
  url: string,
  active?: boolean,
  callback?: (_name: string) => void
}

/**
 * This class represents an abstract navigation bar button.
 */
export default function NavbarComponent(props: Readonly<NavbarComponentProps>) {
  // Callback to bubble up to Toolbar
  const bubbleUp = () => {
    if (props.callback != null) {
      props.callback(props.name);
    }
  }
  return (
    <Link
      className={styles.navbarButton}
      onClick={bubbleUp}
      href={props.url}>

      <Tooltip
        title={props.tooltip}
        enterDelay={1000}
        leaveDelay={100}
        placement="bottom-start">
        <Icon
          className={`
            material-symbols-outlined
            ${styles.image}
            ${props.active ? styles.active : null}`
          }>
          {props.icon}
        </Icon>
      </Tooltip>
    </Link>
  );
}
