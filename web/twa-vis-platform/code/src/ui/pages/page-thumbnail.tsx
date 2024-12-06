"use client";

import styles from './page-thumbnail.module.css';

import { Tooltip } from '@mui/material';
import React from 'react';

import { Assets } from 'io/config/assets';
import { OptionalPage } from 'io/config/optional-pages';
import Image from 'next/image';
import Link from 'next/link';

// Interface for incoming parameters
interface MarkdownPageThumbnailProps {
  page: OptionalPage;
}

export interface DefaultPageThumbnailProps {
  title?: string;
  caption?: string;
  icon?: string;
  url: string;
}

interface PageThumbnailTemplateProps {
  header: string;
  description: string;
  icon: string;
  redirectUrl: string;
}
const ASSET_PREFIX = process.env.ASSET_PREFIX ?? "";

/**
 * A thumbnail that is constructed from the markdown page input. 
 * When clicked, the thumbnail will redirect accordingly.
 * 
 * @param {OptionalPage} page Markdown page content.
 */
export function MarkdownPageThumbnail({ page }: Readonly<MarkdownPageThumbnailProps>) {
  const thumbnail = page.thumbnail ?? Assets.INFO;
  const url = `${ASSET_PREFIX}/${page.slug}`;

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
      key={props.title}
      header={props.title}
      description={props.caption}
      icon={props.icon}
      redirectUrl={props.url}
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
        <div className={styles.thumbnail}>
          <Image src={icon} height={50} width={50} alt={imageDescription} />
        </div>
        <div ref={ref} {...rest} className={styles.content}>
          <h3 className={styles.title}>
            {header}
          </h3>
          <div className={styles.description} >
            {description}
          </div>
        </div>
      </Link >
    );
  });
