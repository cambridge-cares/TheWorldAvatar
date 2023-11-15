import styles from "./css/static-page-thumbnail.module.css";

// Interface for incoming parameters
interface Props {
    title: string,
    description?: string
}

/**
 * Component that represents a clickable thumbnail showing the
 * title and description of a static content page.
 * 
 * @param title Title for content page.
 * @param description Description for content page.
 */
export default function StaticPageThumbnail({ title, description }: Readonly<Props>) {

    return (
        <div className={styles.container}>
            <div className={styles.title}>
                {title}
            </div>
            <div className={styles.description}>
                {description}
            </div>
        </div>
    );
}