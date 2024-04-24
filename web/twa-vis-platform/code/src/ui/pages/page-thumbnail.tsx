"use client";

import styles from './page-thumbnail.module.css';

import React from 'react';
import { Tooltip } from '@mui/material';

import { OptionalPage } from 'io/config/optional-pages';
import AppLink from 'ui/navigation/link/link';
import AppImage from 'ui/graphic/image/image';

// Interface for incoming parameters
interface MarkdownPageThumbnailProps {
  page: OptionalPage;
}

interface DefaultPageThumbnailProps {
  title: string;
  description: string;
  icon: string;
  redirectUrl: string;
}

interface PageThumbnailTemplateProps {
  title: string;
  description: string;
  tooltipText: string;
  icon: string;
  redirectUrl: string;
}

/**
 * A thumbnail that is constructed from the markdown page input. 
 * When clicked, the thumbnail will redirect accordingly.
 * 
 * @param {OptionalPage} page Markdown page content.
 */
export function MarkdownPageThumbnail({ page }: Readonly<MarkdownPageThumbnailProps>) {
  const tooltipText = "Click to open the '" + page.title + "' page.";

  const thumbnail = page.thumbnail ?? "/images/defaults/icons/info.svg";
  const url = `/posts/${page.slug}`;

  return (
    <PageThumbnailTemplate
      title={page.title}
      description={page.description}
      tooltipText={tooltipText}
      icon={thumbnail}
      redirectUrl={url}
    />
  );
}

/**
 * A default page thumbnail that is always available and can redirect to the specified url when clicked.
 * 
 * @param {string} title Title.
 * @param {string} description Description.
 * @param {string} icon Icon to display.
 * @param {string} redirectUrl Redirects to this url when clicked.
 */
export function DefaultPageThumbnail(props: Readonly<DefaultPageThumbnailProps>): React.ReactElement {
  const tooltipText = "Open the '" + props.title + "' page.";

  return (
    <PageThumbnailTemplate
      title={props.title}
      description={props.description}
      tooltipText={tooltipText}
      icon={props.icon}
      redirectUrl={props.redirectUrl}
    />
  );
}

/**
 * A component template that can be reused for page thumbnail components.
 * 
 * @param {string} title Title.
 * @param {string} description Description.
 * @param {string} tooltipText Tool tip text when hovering the thumbnail.
 * @param {string} icon Icon to display.
 * @param {string} redirectUrl Redirects to this url when clicked.
 */
function PageThumbnailTemplate(props: Readonly<PageThumbnailTemplateProps>): React.ReactElement {
  const imageDescription = "Thumbnail icon for the '" + props.title + "' page.";

  return (
    <Tooltip title={props.tooltipText} enterDelay={1000} leaveDelay={100}>
      <AppLink url={props.redirectUrl} className={styles.container}>
        <AppImage url={props.icon} height={50} width={50} alt={imageDescription} classes={styles.thumbnail} />
        <div className={styles.content}>
          <div className={styles.title}>
            <h1>{props.title}</h1>
          </div>
          <div className={styles.description}>
            {props.description}
          </div>
        </div>
      </AppLink>
    </Tooltip>
  );
}