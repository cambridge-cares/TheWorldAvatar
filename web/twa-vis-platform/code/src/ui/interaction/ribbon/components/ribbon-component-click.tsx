"use client";

import styles from './ribbon-component.module.css';

import React from 'react';
import { Tooltip } from '@mui/material';

import IconComponent from 'ui/graphic/icon/icon';

interface RibbonComponentClickProps {
  id: string
  icon: string,
  text?: string,
  tooltip: string,
  action: () => void
}

export default function RibbonComponentClick(props: Readonly<RibbonComponentClickProps>) {
  return (
    <div className={styles.ribbonComponent} onClick={props.action}>
      <Tooltip
        title={props.tooltip}
        enterDelay={1000}
        leaveDelay={100}
        placement="bottom-start">
        <div>
          <div className={styles.ribbonComponentInner}>
            <div className={styles.ribbonComponentIcon}>
              <IconComponent icon={props.icon} />
            </div>
            {props.text &&

              <div className={styles.ribbonComponentText}>
                {props.text}
              </div>
            }
          </div>
        </div>
      </Tooltip>
    </div>
  );
}