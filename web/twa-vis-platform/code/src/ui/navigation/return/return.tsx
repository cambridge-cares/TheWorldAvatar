import styles from './return.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React from 'react';

import { Routes } from 'io/config/routes';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import AppLink from 'ui/navigation/link/link';

/**
 * A button that returns back to the home page.
 * 
 * @returns A return button element to the home page.
 */
export default function ReturnButton() {
  return (
    <AppLink url={Routes.HOME} className={styles.button}>
      <MaterialIconButton
        iconName="arrow_circle_left"
        iconStyles={[iconStyles["large-icon"]]}
        className={iconStyles["elongated-icon-button-container"]}
      />
    </AppLink>
  )
}