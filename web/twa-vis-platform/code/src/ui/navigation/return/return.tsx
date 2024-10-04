import iconStyles from 'ui/graphic/icon/icon-button.module.css';
import styles from './return.module.css';

import { useRouter } from 'next/navigation';

import MaterialIconButton from 'ui/graphic/icon/icon-button';

interface ReturnButtonProps {
  styles?: string;
}

/**
 * A button that returns back to the home page.
 * @param {string} styles Additional styles for return button if required.
 * 
 * @returns A return button element to the home page.
 */
export default function ReturnButton(props: Readonly<ReturnButtonProps>) {
  const router = useRouter();

  return (
    <button type="button" className={`${styles.button} ${props.styles}`} onClick={() => router.back()}>
      <MaterialIconButton
        iconName="arrow_circle_left"
        iconStyles={[iconStyles["large-icon"]]}
        className={iconStyles["elongated-icon-button-container"]}
      />
    </button>
  )
}