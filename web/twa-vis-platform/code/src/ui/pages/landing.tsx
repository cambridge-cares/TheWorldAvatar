/**
 * Optional landing page.
 */

import 'github-markdown-css/github-markdown.css';
import styles from './landing.module.css';

import markdownit from 'markdown-it';
import React from 'react';

import OptionalPages, { OptionalPage } from 'io/config/optional-pages';
import { Routes } from 'io/config/routes';
import CredoImage from 'ui/graphic/image/CredoImage';
import { DefaultPageThumbnail, MarkdownPageThumbnail } from './page-thumbnail';
import { DefaultSettings } from 'types/settings';

// Utilities to render markdown into HTML
const markdowner = markdownit({
    html: true,
    typographer: true,
    breaks: true,
    linkify: true
});

interface LandingPageProps {
    settings: DefaultSettings,
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
                <CredoImage />
                {thumbnails}
                {props.settings.modules.map && (
                    <DefaultPageThumbnail
                        title="CReDo"
                        caption="Use CReDo to understand the climate resilience of your infrastructure network"
                        icon="./images/defaults/icons/map.svg"
                        url={Routes.MAP}
                    />
                )}
                {props.settings.modules.dashboard && (
                    <DefaultPageThumbnail
                        title="Analyse"
                        caption="Discover trends and insights at a glance"
                        icon="./images/defaults/icons/dash.svg"
                        url={Routes.DASHBOARD}
                    />
                )}
                {props.settings.modules.acknowledgements && (
                    <DefaultPageThumbnail
                        title="Acknowledgements"
                        caption="Discover the CReDo partners and contributors"
                        icon="./images/defaults/icons/glossary.svg"
                        url={Routes.ACKNOWLEDGEMENTS}
                    />
                )}
                <DefaultPageThumbnail
                    title="Help Centre"
                    caption="Get help with the CReDo app"
                    icon="./images/defaults/icons/twa.svg"
                    url={Routes.HELP}
                />

                {props.settings.external?.map((externalLink, index) =>
                    <DefaultPageThumbnail
                        key={externalLink.title + index}
                        title={externalLink.title}
                        caption={externalLink.caption}
                        icon={externalLink.icon}
                        url={externalLink.url}
                    />)}
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