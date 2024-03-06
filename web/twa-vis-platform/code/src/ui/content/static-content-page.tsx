/**
 * Single page used to show static, user-generated content pulled
 * from a markdown file.
 */

"use client";

import styles from './static-content-page.module.css';
import 'github-markdown-css/github-markdown.css';

import Link from 'next/link';
import React from 'react';
import { ReactNode } from 'react';

import MaterialIconButton from 'ui/buttons/icon-button';

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

    // Return to landing button
    const returnButton = (
        <Link href="/" className={styles.button}>
            <MaterialIconButton
                iconName="arrow_circle_left"
            />
        </Link>
    );

    if(childNodes != null) {
        return (
            <div className={styles.container} key="static-content-page">
                {returnButton}
                <div className={styles.contentOuter}>
                    <div className={classNames}>
                        {childNodes}
                    </div>
                </div>
            </div>
        );
    } else if(childString != null) {
        return (
            <div className={styles.container} key="static-content-page">
                {returnButton}
                <div className={styles.contentOuter}>
                    <div
                        className={classNames}
                        dangerouslySetInnerHTML={{__html: childString}}
                    />
                </div>
            </div>
        );
    }
}