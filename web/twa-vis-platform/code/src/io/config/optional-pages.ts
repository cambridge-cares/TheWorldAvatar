import fs from 'fs';
import path from 'path';
import matter from 'gray-matter';

/**
 * Type definition for an optional page.
 */
export type OptionalPage = {
    title: string,
    slug: string, 
    description: string,
    content: string,
    filename: string,
    thumbnail?: string
}

/**
 * Handles loading routes and contents for additional/optional pages
 * with static content (i.e. acknowledgements, glossaries, legends etc.).
 */
export default class OptionalPages {

    // Location of optional pages directory.
    private static readonly DIRECTORY:string = "public/optional-pages";

    /**
     * 
     */
    private static readonly LOADED_PAGES: Array<OptionalPage> = [];
    
    /**
     * Private constructor.
     */
    private constructor() {
        // Empty.
    }

    /**
     * Reads the contents of all "*.md" files within the "config/optional-pages"
     * directory, ready for dynamic routing.
     * 
     * @param dir optional config directory path (relative to root).
     */
    public static loadPages(dir?: string) {
        if(OptionalPages.LOADED_PAGES.length > 0) return;

        try {
            // List file names from pages directory.
            const pagesDirectory = path.join(process.cwd(), dir ?? this.DIRECTORY);
            const fileNames = fs.readdirSync(pagesDirectory);

            // Parse each file and key under file name (without extension).
            fileNames.forEach((fileName) => {
                if(fileName.endsWith(".md")) {
                    const fullPath = path.join(pagesDirectory, fileName);
                    const rawContents = fs.readFileSync(fullPath, "utf8");
                    const matterResult = matter(rawContents);

                    const page: OptionalPage = {
                        title: matterResult.data.title,
                        slug: matterResult.data.slug,
                        description: matterResult.data.description,
                        content: formatMarkdownImages(matterResult.content),
                        filename: fileName,
                        thumbnail: matterResult.data.thumbnail
                    };
                    OptionalPages.LOADED_PAGES.push(page);
                }
            });

            // Sort pages by filename
            OptionalPages.LOADED_PAGES.sort(OptionalPages.compare);
            
            console.info("Loaded optional pages content.");
        } catch(error) {
            console.error("Could not read any optional pages!", error);
        }
        return OptionalPages.LOADED_PAGES;
    }

    /**
     * Returns cached optional page content.
     * 
     * @param slug ID of optional page.
     */
    public static getPage(slug: string): OptionalPage {
        if(OptionalPages.LOADED_PAGES.length == 0) OptionalPages.loadPages();

        for(const page of OptionalPages.LOADED_PAGES) {
             if(page.slug === slug) return page;
        }
        return null;
    }

    /**
     * Returns all cached optional pages.
     * 
     * @returns array of OptionalPage instances.
     */
    public static getAllPages(): OptionalPage[] {
        return [...OptionalPages.LOADED_PAGES];
    } 

    /**
     * Compares OptionalPage instances by filename.
     * 
     * @param objA object a.
     * @param objB object b.
     * @returns comparison result.
     */
    private static compare(objA: OptionalPage, objB: OptionalPage ): number {
        if ( objA.filename < objB.filename ){
          return -1;
        }
        if ( objA.filename > objB.filename ){
          return 1;
        }
        return 0;
    }

}
// End of class.

/**
 * Format the images in the markdown content input with a prefix if there is a base path set.
 * 
 * @param markdownContent Entire content of markdown document.
 * @returns The input content with prefix added to image urls.
 */
function formatMarkdownImages(markdownContent: string): string {
    const prefix: string = process.env.BASE_PATH || "/";
    // Append base path if available
    // Formats either <img src="/images/... or <img src="images/...
    // Exclude any initial img elements starting with http 
    return markdownContent.replace(/(<img\s+src=")(?!https?:\/\/)(\/)?([^"]+")/g, `$1${prefix}$3`);
}