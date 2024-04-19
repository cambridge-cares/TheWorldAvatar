/**
 * Single page used to show static, user-generated content pulled
 * from a markdown file.
 */

"use client";

import styles from './static-content-page.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';
import 'github-markdown-css/github-markdown.css';

import React, { ReactNode } from 'react';

import { Routes } from 'io/config/routes';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import AppLink from 'ui/navigation/link/link';

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
        <AppLink url={Routes.HOME} className={styles.button}>
            <MaterialIconButton
                iconName="arrow_circle_left"
                iconStyles={[iconStyles["large-icon"]]}
                className={iconStyles["elongated-icon-button-container"]}
            />
        </AppLink>
    );

    if (childNodes != null) {
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
    } else if (childString != null) {
        return (
            <div className={styles.container} key="static-content-page">
                {returnButton}
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