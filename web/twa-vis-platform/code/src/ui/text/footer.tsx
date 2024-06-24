import styles from './footer.module.css';

import React from 'react';

import Link from 'next/link';

/**
 * Renders a footer.
 */
export default function Footer() {
  return (
    <footer className={styles.footer}>
      <span>Powered by&nbsp;</span>
      <Link href="https://theworldavatar.io">The World Avatar</Link>
    </footer>
  );
}