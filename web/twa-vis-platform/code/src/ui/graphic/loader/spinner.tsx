import React from 'react';

import styles from './loader.module.css';

interface LoadingSpinnerProps {
  isSmall: boolean;
}

/**
 * This component renders a loading spinner.
 * 
 * @param {boolean} isSmall Indicates if the loading spinner should be small or not.
 */
export default function LoadingSpinner(props: Readonly<LoadingSpinnerProps>) {
  const sizeStyles: string = props.isSmall ? styles.small : styles.default;
  return <div className={`${styles.spinner} ${sizeStyles}`}></div>
}