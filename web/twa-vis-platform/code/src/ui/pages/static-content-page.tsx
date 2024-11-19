/**
 * Single page used to show static, user-generated content pulled
 * from a markdown file.
 */

"use client";

import styles from './static-content-page.module.css';
import returnButtonStyles from '../navigation/return/return.module.css';

import 'github-markdown-css/github-markdown.css';

import React, { ReactNode } from 'react';

import ReturnButton from 'ui/navigation/return/return';

// Interface for properties with react nodes
interface Props {
  childNodes?: ReactNode,
  childString?: string
}

/**
 * Component that represents a single page with static content.
 * Commonly used for glossaries, legends, acknowledgements etc.
 * 
 * @param childNodes React nodes to add to this component.
 * @param childString HTML string to add to this component.
 */
export default function StaticContentPage({ childNodes, childString }: Readonly<Props>) {
  // CSS class names
  const classNames = ["markdown-body", styles.contentInner].join(" ");

  if (childNodes != null) {
    return (
      <div className={styles.container} key="static-content-page">
        <ReturnButton />
        <div className={styles.contentOuter}>
          <div className={classNames}>
            {childNodes}
          </div>
        </div>
      </div>
    );
  } else if (childString != null) {
    return (
      <div className={styles.container} key="static-content-page">
        <ReturnButton styles={returnButtonStyles["button-padding"]} />
        <div className={styles.contentOuter}>
          <div
            className={classNames}
            dangerouslySetInnerHTML={{ __html: childString }}
          />
        </div>
      </div>
    );
  }
}