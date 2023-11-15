import { ReactNode } from "react";
import "github-markdown-css/github-markdown.css";

import styles from "./css/static-content-page.module.css";

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
    let classNames = ["markdown-body", styles.content].join(" ");

    if(childNodes != null) {
        return (
            <div className={styles.container}>
                <div className={classNames}>
                    {childNodes}
                </div>
            </div>
        );
    } else if(childString != null) {
        return (
            <div className={styles.container}>
                <div
                    className={classNames}
                    dangerouslySetInnerHTML={{__html: childString}}
                />
            </div>
        );
    }
}