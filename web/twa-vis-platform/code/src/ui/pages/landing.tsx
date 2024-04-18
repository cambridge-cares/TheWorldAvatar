/**
 * Optional landing page.
 */

import styles from './landing.module.css';
import 'github-markdown-css/github-markdown.css';

import React from 'react';
import Link from 'next/link';
import { Tooltip } from '@mui/material';
import markdownit from 'markdown-it';

import { Routes } from 'io/config/routes';
import OptionalPages, { OptionalPage } from 'io/config/optional-pages';
import StaticPageThumbnail from './static-page-thumbnail';
import AppLink from 'ui/navigation/link/link';
import IconComponent from 'ui/graphic/icon/icon';

// Utilities to render markdown into HTML
const markdowner = markdownit({
    html: true,
    typographer: true,
    breaks: true,
    linkify: true
});

type LandingPageProps = {
    hasMap: boolean,
    hasDashboard: boolean,
}

/**
 * Represents a standard landing page for the TWA platform. Can optionally
 * contain dynamically created links to other static pages (e.g. acknowledgements,
 * glossaries, licensing etc.).
 * 
 * @returns JSX for landing page.
 */
export default function LandingPage(props: LandingPageProps) {
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
        "/images/defaults/icons/map.svg",
        Routes.MAP
    );

    // Button to open dashboard page.
    const dashButton = buildButton(
        "Analytics dashboard",
        "Investigate all data across all scenarios.",
        "/images/defaults/icons/dash.svg",
        Routes.DASHBOARD
    );

    return (
        <div className={styles.container}>

            <div className={styles.introOuter}>
                <div className={styles.introMiddle}>
                    <div
                        className={introClasses}
                        dangerouslySetInnerHTML={{ __html: introContent }}
                    />

                    <div className={styles.introLower}>
                        {props.hasMap && mapButton}
                        {props.hasDashboard && dashButton}
                    </div>
                    <footer className={styles.footer}>
                        <span>Powered by </span>
                        <Link href="https://theworldavatar.io">The World Avatar</Link>
                    </footer>
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
            <AppLink url={url} className={styles.button}>
                {/* Add thumbnail */}
                <IconComponent icon={icon} classes={styles.buttonIcon} />
                {/* Page title */}
                <div className={styles.buttonTitle}>
                    <h1>{title}</h1>
                </div>
                {/* Page description */}
                <div className={styles.buttonDescription}>
                    {description}
                </div>
            </AppLink>
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