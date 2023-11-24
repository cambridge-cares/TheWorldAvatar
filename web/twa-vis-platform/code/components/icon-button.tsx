"use client";

import Icon from '@mui/material/Icon';
import styles from "./css/icon-button.module.css";

// Interface for properties
interface Props {
    iconName: string,
    callback?: () => void
}

/**
 * 
 * @param param0 
 */
export default function MaterialIconButton({ iconName, callback }: Readonly<Props>) {
    // CSS classes for icons
    let classNames = ["material-symbols-outlined", styles.iconButton].join(" ");

    return (
        <div className={styles.iconButtonContainer}>
            <Icon className={classNames} onClick={callback}>
                {iconName}
            </Icon>
        </div>
    );
}