import iconStyles from 'ui/graphic/icon/icon-button.module.css';
import styles from './return.module.css';


import { Routes } from 'io/config/routes';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import Link from 'next/link';

/**
 * A button that returns back to the home page.
 * 
 * @returns A return button element to the home page.
 */
export default function ReturnButton() {
  return (
    <Link href={Routes.HOME} className={styles.button}>
      <MaterialIconButton
        iconName="arrow_circle_left"
        iconStyles={[iconStyles["large-icon"]]}
        className={iconStyles["elongated-icon-button-container"]}
      />
    </Link>
  )
}