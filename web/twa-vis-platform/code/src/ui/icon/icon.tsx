import React from 'react';
import SVG from 'react-inlinesvg';
import { Icon } from '@mui/material';

interface IconComponentProps {
  readonly icon: string;
  readonly classes: string
}

/**
 * Reusable component for displaying icons. It supports PNG, JPG, SVG, and Google Material icons.
 * 
 * @param {string} icon The icon to display. It can be a URL to an image (PNG, JPG), the name of a Material icon, or the path to an SVG.
 * @param {string} classes Additional CSS classes to apply to the icon element.
 */
export default function IconComponent(props: IconComponentProps) {
  if (props.icon.endsWith(".png") || props.icon.endsWith(".jpg")) {
    return (
      <img className={props.classes} src={props.icon} alt = "Icon"/>
    );
  } else if (props.icon.endsWith(".svg")) {
    return (
      <SVG className={props.classes} src={props.icon} />
    );
  } else {
    // Name of Google material icon
    return (
      <Icon className={props.classes}>
        {props.icon}
      </Icon>
    );
  }
}