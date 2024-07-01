import iconStyles from 'ui/graphic/icon/icon-button.module.css';
import styles from './return.module.css';

import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

/**
 * A button that returns back to the home page.
 * 
 * @returns A return button element to the home page.
 */
export default function ReturnButton() {
  const router = useRouter();

  return (
    <button type="button" className={styles.button} onClick={() => router.back()}>
      <MaterialIconButton
        iconName="arrow_circle_left"
        iconStyles={[iconStyles["large-icon"]]}
        className={iconStyles["elongated-icon-button-container"]}
      />
    </button>
  )
}