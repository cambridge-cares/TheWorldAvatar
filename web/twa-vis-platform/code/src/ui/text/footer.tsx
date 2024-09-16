import styles from './footer.module.css'
import Link from 'next/link'
import Image from 'next/image'

/**
 * Renders a footer.
 */
export default function Footer() {
  return (
    <footer className={styles.footer}>
      <Image alt={"TWA Logo"} src={"/images/defaults/icons/twa.svg"} width={30} height={30} style={{ paddingRight: 5 }} />
      <span>Powered by&nbsp;
        <Link href="https://theworldavatar.io">The World Avatar</Link>
      </span>
    </footer >
  );
}