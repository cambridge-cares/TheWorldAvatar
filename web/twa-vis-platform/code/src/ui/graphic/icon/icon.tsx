import { Icon } from '@mui/material';
import SVG from 'react-inlinesvg';

import AppImage from 'ui/graphic/image/image';

interface IconComponentProps {
  readonly icon: string;
  readonly classes?: string
  readonly height?: number
  readonly width?: number
}

/**
 * Reusable component for displaying icons. It supports PNG, JPG, SVG, and Google Material icons.
 * 
 * @param {string} icon The icon to display. It can be a URL to an image (PNG, JPG), the name of a Material icon, or the path to an SVG.
 * @param {string} classes Additional CSS classes to apply to the icon element if it is not PNG or JPG.
 * @param {number} height Maximum rendered height in pixels for only PNG and JPG inputs.
 * @param {number} width Maximum rendered width in pixels for only PNG and JPG inputs.
 */
export default function IconComponent(props: IconComponentProps) {
  if (props.icon.endsWith(".png") || props.icon.endsWith(".jpg")) {
    return (
      <AppImage
        url={props.icon}
        classes={props.classes}
        alt="Icon" />
    );
  } else if (props.icon.endsWith(".svg")) {
    return (
      <SVG className={props.classes} src={props.icon} />
    );
  } else {
    const iconClassNames = ["material-symbols-outlined"].concat(props.classes).join(" ");
    // Name of Google material icon
    return (
      <Icon className={iconClassNames}>
        {props.icon}
      </Icon>
    );
  }
}