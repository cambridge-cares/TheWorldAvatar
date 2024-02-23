import { Metadata } from "next";

import StaticContentPage from "../../../ui/content/static-content-page";
import { OptionalPage, OptionalPages } from "io/config/optional-pages";

// Type definition for incoming page parameters
type Properties = {
    params: {
        id: string
    }
}

// Utilities to render markdown into HTML
const markdowner = require("markdown-it")({
    html: true,
    typographer: true,
    breaks: true,
    linkify: true
});

/**
 * Dynamically set page metadata based on the incoming route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata({ params }: Properties): Promise<Metadata> {
    let page: OptionalPage = OptionalPages.getPage(params.id);
    return {
        title: page.title,
        description: page.description
    }
}

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 * 
 * @param params incoming route parameters.
 * 
 * @returns React component for display. 
 */
export default async function Post({ params }: Readonly<Properties>) {
    // Get cached page content.
    let page: OptionalPage = OptionalPages.getPage(params.id);
    let markdownResult = markdowner.render(page.content);

    return (
        <StaticContentPage
            childString={markdownResult}
        />
    )
}