"use client";

import { ReactNode } from "react";
import Link from "next/link";
import "github-markdown-css/github-markdown.css";

import styles from "./css/static-content-page.module.css";
import MaterialIconButton from "./icon-button";

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
    let classNames = ["markdown-body", styles.contentInner].join(" ");

    // Return to landing button
    let returnButton = (
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