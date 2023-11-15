import "github-markdown-css/github-markdown.css";

import styles from "./landing.module.css";
import ModuleToggle from "@/utils/settings/module-toggle";
import { OptionalPage, OptionalPages } from "@/utils/settings/optional-pages";

// Utilities to render markdown into HTML
const markdowner = require("markdown-it")({
    html: true,
    typographer: true,
    breaks: true,
    linkify: true
});

/**
 * Represents a standard landing page for the TWA platform. Can optionally
 * contain dynamically created links to other static pages (e.g. acknowledgements,
 * glossaries, licensing etc.).
 * 
 * @returns JSX for landing page.
 */
export default function LandingPage() {
    // CSS class names
    let introClasses = ["markdown-body", styles.introContent].join(" ");

    // Get landing page content
    let content  = getLandingContent();

    // Render intro content to HTML
    let introContent = markdowner.render(content);

    return (
        <div className={styles.container}>
            <div className={styles.introContainer}>
                <div
                    className={introClasses}
                    dangerouslySetInnerHTML={{__html: introContent}}
                />
            </div>

            <div className={styles.thumbnailContainer} />
        </div>
    )
}

function getLandingContent(): string {
    let page: OptionalPage = OptionalPages.getPage(ModuleToggle.LANDING.key);
    return page.content;
}