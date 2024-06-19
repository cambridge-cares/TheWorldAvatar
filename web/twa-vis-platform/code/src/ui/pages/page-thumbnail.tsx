"use client";

import styles from './page-thumbnail.module.css';

import React from 'react';
import { Tooltip } from '@mui/material';

import { OptionalPage } from 'io/config/optional-pages';
import AppImage from 'ui/graphic/image/image';
import Link from 'next/link';

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
  header: string;
  description: string;
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
  const thumbnail = page.thumbnail ?? "/images/defaults/icons/info.svg";
  const url = `/posts/${page.slug}`;

  return (
    <PageThumbnailTemplate
      header={page.title}
      description={page.description}
      icon={thumbnail}
      redirectUrl={url}
    />
  );
}

/**
 * A default page thumbnail that is always available and can redirect to the specified url when clicked.
 * 
 * @param {string} title Thumbnail title.
 * @param {string} description Description.
 * @param {string} icon Icon to display.
 * @param {string} redirectUrl Redirects to this url when clicked.
 */
export function DefaultPageThumbnail(props: Readonly<DefaultPageThumbnailProps>): React.ReactElement {
  return (
    <PageThumbnailTemplate
      header={props.title}
      description={props.description}
      icon={props.icon}
      redirectUrl={props.redirectUrl}
    />
  );
}

/**
 * A component template that can be reused for page thumbnail components.
 * 
 * @param {string} header Title for thumbnail.
 * @param {string} description Description.
 * @param {string} icon Icon to display.
 * @param {string} redirectUrl Redirects to this url when clicked.
 */
function PageThumbnailTemplate(props: Readonly<PageThumbnailTemplateProps>): React.ReactElement {
  const tooltipText = "Click to open the '" + props.header + "' page.";
  // In order to add custom child components to MUI's tooltip component, we need to forward the Ref to prevent errors
  return (
    <Tooltip title={tooltipText} enterDelay={1000} leaveDelay={100}>
      <ForwardedPageThumbnailTemplate header={props.header}
        description={props.description}
        icon={props.icon}
        redirectUrl={props.redirectUrl} />
    </Tooltip>
  );
}

const ForwardedPageThumbnailTemplate = React.forwardRef<HTMLDivElement, Readonly<PageThumbnailTemplateProps>>(
  function ForwardedPageThumbnailTemplate({ header, description, icon, redirectUrl, ...rest }, ref): React.ReactElement {
    const imageDescription = "Thumbnail icon for the '" + header + "' page.";
    return (
      <Link href={redirectUrl} className={styles.container}>
        <AppImage url={icon} height={50} width={50} alt={imageDescription} classes={styles.thumbnail} />
        <div ref={ref} {...rest} className={styles.content}>
          <div className={styles.title}>
            <h1>{header}</h1>
          </div>
          <div className={styles.description}>
            {description}
          </div>
        </div>
      </Link>
    );
  });
