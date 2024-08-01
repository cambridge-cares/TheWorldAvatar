import React from 'react';

import markdownIt from "markdown-it";

import StaticContentPage from 'ui/pages/static-content-page';
import OptionalPages, { OptionalPage } from 'io/config/optional-pages';

// Utilities to render markdown into HTML
const markdowner = markdownIt({
  html: true,
  typographer: true,
  breaks: true,
  linkify: true
});

/**
 * Renders the help page.
 */
export default function HelpPage() {
  const page: OptionalPage = OptionalPages.getPage("help");
  const markdownResult = markdowner.render(page.content);

  return (
    <StaticContentPage
      childString={markdownResult}
    />
  )
}