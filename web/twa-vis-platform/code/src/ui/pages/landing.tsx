/**
 * Optional landing page.
 */

import styles from "./landing.module.css";
import "github-markdown-css/github-markdown.css";

import React from "react";
import Link from "next/link";
import { Tooltip } from "@mui/material";
import markdownit from "markdown-it";

import StaticPageThumbnail from "ui/content/static-page-thumbnail";
import { OptionalPage, OptionalPages } from "io/config/optional-pages";

// Utilities to render markdown into HTML
const markdowner = markdownit({
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
    const introClasses = ["markdown-body", styles.introInner].join(" ");

    // Get landing page introduction content
    const introContent = getIntroductionContent();

    // Get thumbnails for static content pages
    const thumbnails = getThumbnails();

    // Button to open map page.
    const mapButton = buildButton(
        "Geospatial map",
        "Explore assets, connections, and failures visually.",
        "/img/icons/map.svg",
        "/visualisation"
    );

    // Button to open dashboard page.
    const dashButton = buildButton(
        "Analytics dashboard",
        "Investigate all data across all scenarios.",
        "/img/icons/dash.svg",
        "/analytics"
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

/**
 * Build a custom button to navigate to another page.
 * 
 * @param title page title. 
 * @param description page description.
 * @param icon image icon.
 * @param url page URL.
 * 
 * @returns generated React element.
 */
function buildButton(title: string, description: string, icon: string, url: string): React.ReactElement {
    const tooltipText = "Open the '" + title + "' page.";

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
    const page: OptionalPage = OptionalPages.getPage("landing");
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
    const components: React.ReactElement[] = [];
    pages.forEach(page => {
        const thumbnail = (
            <StaticPageThumbnail
                page={page}
            />
        );
        components.push(thumbnail);
    });

    return components;
}