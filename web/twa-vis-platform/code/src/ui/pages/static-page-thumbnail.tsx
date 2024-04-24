"use client";

import styles from './static-page-thumbnail.module.css';

import React from 'react';
import { Tooltip } from '@mui/material';

import { OptionalPage } from 'io/config/optional-pages';
import AppLink from 'ui/navigation/link/link';
import AppImage from 'ui/graphic/image/image';

// Interface for incoming parameters
interface Props {
    page: OptionalPage
}

/**
 * Component that represents a clickable thumbnail showing the
 * title and description of a static content page.
 * 
 * @param title Title for content page.
 * @param description Description for content page.
 */
export default function StaticPageThumbnail({ page }: Readonly<Props>) {

    const tooltipText = "Click to open the '" + page.title + "' page.";
    const imageDescription = "Thumbnail icon for the '" + page.title + "' page.";

    const thumbnail = page.thumbnail ?? "/images/defaults/icons/info.svg";
    const url = `/posts/${page.slug}`;

    return (
        <Tooltip title={tooltipText} enterDelay={1000} leaveDelay={100}>
            <AppLink url={url} className={styles.container}>
                {/* Add thumbnail */}
                <AppImage url={thumbnail} height={50} width={50} alt={imageDescription} classes={styles.thumbnail} />

                <div className={styles.content}>
                    <div className={styles.title}>
                        <h1>{page.title}</h1>
                    </div>
                    <div className={styles.description}>
                        {page.description}
                    </div>
                </div>
            </AppLink>
        </Tooltip>
    );
}