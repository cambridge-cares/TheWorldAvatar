import fs from "fs";
import path from "path";
import matter from "gray-matter";

/**
 * Type definition for an optional page.
 */
export type OptionalPage = {
    title: string,
    slug: string, 
    description: string,
    content: string
}

/**
 * Handles loading routes and contents for additional/optional pages
 * with static content (i.e. acknowledgements, glossaries, legends etc.).
 */
export class OptionalPages {

    // Location of optional pages directory.
    private static readonly DIRECTORY = "../config/optional-pages";

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
            let pagesDirectory = path.join(process.cwd(), dir ?? this.DIRECTORY);
            const fileNames = fs.readdirSync(pagesDirectory);

            // Parse each file and key under file name (without extension).
            fileNames.forEach((fileName) => {
                if(fileName.endsWith(".md")) {
                    const fullPath = path.join(pagesDirectory, fileName);
                    const rawContents = fs.readFileSync(fullPath, "utf8");
                    const matterResult = matter(rawContents);

                    let page: OptionalPage = {
                        title: matterResult.data.title,
                        slug: matterResult.data.slug,
                        description: matterResult.data.description,
                        content: matterResult.content
                    };
                    OptionalPages.LOADED_PAGES.push(page);
                }
            });
            console.debug("Loaded optional pages content.");
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

        for(let page of OptionalPages.LOADED_PAGES) {
             if(page.slug === slug) return page;
        }
        return null;
    }

}
// End of class.