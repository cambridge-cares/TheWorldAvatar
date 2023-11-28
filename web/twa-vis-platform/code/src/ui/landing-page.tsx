import styles from "./css/landing.module.css";
import "github-markdown-css/github-markdown.css";

import Link from "next/link";
import { Tooltip } from "@mui/material";

import StaticPageThumbnail from "ui/static-page-thumbnail";
import { OptionalPage, OptionalPages } from "io/config/optional-pages";

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
    let introClasses = ["markdown-body", styles.introInner].join(" ");

    // Get landing page introduction content
    let introContent = getIntroductionContent();

    // Get thumbnails for static content pages
    let thumbnails = getThumbnails();

    let mapButton = buildButton(
        "Geospatial map",
        "Explore assets, connections, and failures visually.",
        "/img/icons/map.svg",
        "/map"
    );

    let dashButton = buildButton(
        "Analytics dashboard",
        "Investigate all data across all scenarios.",
        "/img/icons/dash.svg",
        "/dashboard"
    );

    return (
        <div className={styles.container}>

            <div className={styles.introOuter}>
                <div className={styles.introMiddle}>
                    <div
                        className={introClasses}
                        dangerouslySetInnerHTML={{__html: introContent}}
                    />

                    <div className={styles.introLower}>
                        {mapButton}
                        {dashButton}
                    </div>
                </div>
            </div>

            <div className={styles.thumbnailContainer}>
                {thumbnails}
            </div>
        </div>
    )
}

function buildButton(title: string, description: string, icon: string, url: string): React.ReactElement {
    let tooltipText = "Open the '" + title + "' page.";

    return (
        <Tooltip title={tooltipText} enterDelay={1000} leaveDelay={100}>
            <Link href={url} className={styles.button}>
                {/* Add thumbnail */}
                <div className={styles.buttonIcon}>
                    <img src={icon}/> 
                </div>
                {/* Page title */}
                <div className={styles.buttonTitle}>
                    <h1>{title}</h1>
                </div>
                {/* Page description */}
                <div className={styles.buttonDescription}>
                    {description}
                </div>
            </Link>
        </Tooltip>
    )
}

/**
 * Grabs introduction content from the optional page defining the landing.
 * 
 * @returns Introduction HTML content.
 */
function getIntroductionContent(): string {
    let page: OptionalPage = OptionalPages.getPage("landing");
    return markdowner.render(page.content);
}

/**
 * Loads linkable thumbnail components for registered
 * optional static content pages.
 * 
 * @returns Array of thumbnail components.
 */
function getThumbnails(): React.ReactElement[] {
    // Get all pages
    let pages = OptionalPages.getAllPages();

    // Filter out the object that defines the landing page content
    pages = pages.filter(page => page.slug !== "landing");

    // Create thumbnail components for each page
    let components: React.ReactElement[] = [];
    pages.forEach(page => {
        let thumbnail = (
            <StaticPageThumbnail
                page={page}
            />
        );
        components.push(thumbnail);
    });

    return components;
}