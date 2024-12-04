import styles from './error.module.css';

import React from 'react';

interface ErrorComponentProps {
  message: string;
}

/**
 * Renders error message.
 * 
 * @param {string} message An error message to render.
 */
export default function ErrorComponent(props: Readonly<ErrorComponentProps>) {
  return (
    <p className={styles.error}>{`*${props.message}`}</p>
  );
}