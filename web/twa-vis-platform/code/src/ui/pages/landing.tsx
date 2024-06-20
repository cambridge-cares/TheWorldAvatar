/**
 * Optional landing page.
 */

import styles from './landing.module.css';
import 'github-markdown-css/github-markdown.css';

import React from 'react';
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

interface LandingPageProps {
    hasMap: boolean,
    hasDashboard: boolean,
    hasAcknowledgements?: boolean
}

/**
 * Represents a standard landing page for the TWA platform. Can optionally
 * contain dynamically created links to other static pages (e.g. acknowledgements,
 * glossaries, licensing etc.).
 * 
 * @returns JSX for landing page.
 */
export default function LandingPage(props: Readonly<LandingPageProps>) {
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

                </div>
            </div>

            <div className={styles.thumbnailContainer}>
                {thumbnails}
                {props.hasMap && (
                    <DefaultPageThumbnail
                        title="CReDo"
                        description="Use CReDo to understand the climate resilience of your infrastructure network"
                        icon="./images/defaults/icons/map.svg"
                        redirectUrl={Routes.MAP}
                    />
                )}
                {props.hasDashboard && (
                    <DefaultPageThumbnail
                        title="Analyse"
                        description="Discover trends and insights at a glance"
                        icon="./images/defaults/icons/dash.svg"
                        redirectUrl={Routes.DASHBOARD}
                    />
                )}
                {props.hasAcknowledgements && (
                    <DefaultPageThumbnail
                        title="Acknowledgements"
                        description="Discover the CReDo partners and contributors"
                        icon="./images/defaults/icons/glossary.svg"
                        redirectUrl={Routes.ACKNOWLEDGEMENTS}
                    />
                )}
                <DefaultPageThumbnail
                    title="Help Centre"
                    description="Get help with the CReDo app"
                    icon="./images/defaults/icons/twa.svg"
                    redirectUrl={Routes.HELP}
                />
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

    // Filter out the object that defines the landing or help page content
    pages = pages.filter(page => page.slug !== "landing" && page.slug !== "help");

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