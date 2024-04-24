/**
 * Optional landing page.
 */

import styles from './landing.module.css';
import 'github-markdown-css/github-markdown.css';

import React from 'react';
import Link from 'next/link';
import markdownit from 'markdown-it';

import { Routes } from 'io/config/routes';
import OptionalPages, { OptionalPage } from 'io/config/optional-pages';
import { DefaultPageThumbnail, MarkdownPageThumbnail } from './page-thumbnail';

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

    return (
        <div className={styles.container}>

            <div className={styles.introOuter}>
                <div className={styles.introMiddle}>
                    <div
                        className={introClasses}
                        dangerouslySetInnerHTML={{ __html: introContent }}
                    />
                    <footer className={styles.footer}>
                        <span>Powered by </span>
                        <Link href="https://theworldavatar.io">The World Avatar</Link>
                    </footer>
                </div>
            </div>

            <div className={styles.thumbnailContainer}>
                {thumbnails}
                {props.hasMap && (
                    <DefaultPageThumbnail
                        title="Explore"
                        description="Discover geospatial relationships in our environment"
                        icon="/images/defaults/icons/map.svg"
                        redirectUrl={Routes.MAP}
                    />
                )}
                {props.hasDashboard && (
                    <DefaultPageThumbnail
                        title="Analyse"
                        description="Discover trends and insights at a glance"
                        icon="/images/defaults/icons/dash.svg"
                        redirectUrl={Routes.DASHBOARD}
                    />
                )}
            </div>
        </div>
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
            <MarkdownPageThumbnail
                page={page}
            />
        );
        components.push(thumbnail);
    });

    return components;
}