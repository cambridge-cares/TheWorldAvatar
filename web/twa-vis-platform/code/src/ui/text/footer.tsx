import styles from './footer.module.css'
import Link from 'next/link';
import Image from 'next/image';

import { Assets } from 'io/config/assets';

/**
 * Renders a footer.
 */
export default function Footer() {
  return (
    <footer className={styles.footer}>
      <Image alt={"TWA Logo"} src={Assets.TWA} width={30} height={30} style={{ paddingRight: 5 }} />
      <span>Powered by&nbsp;
        <Link href="https://theworldavatar.io">The World Avatar</Link>
      </span>
    </footer >
  );
}