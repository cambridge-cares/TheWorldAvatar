

import styles from './info-tree.module.css';
import iconStyles from 'ui/graphic/icon/icon-button.module.css';

import React, { useState } from 'react';

import { Attribute, AttributeGroup } from 'types/attribute';
import MaterialIconButton from 'ui/graphic/icon/icon-button';
import HeaderField from 'ui/text/header';

// type definition for incoming properties
interface InfoTreeNodeProps {
  attribute: AttributeGroup;
}

interface InfoTreeSubNodeProps {
  group: AttributeGroup;
  depth: number;
}

interface InfoTreeAttributeNodeProps {
  attributes: Attribute[];
  depth: number;
}

/**
 * This component renders the Info Tree based on the input data.
 *
 * @param {AttributeGroup} attribute The attribute group to render.
 */
export default function InfoTreeNode(props: Readonly<InfoTreeNodeProps>) {
  // This component is separate from the SubNode as the root node should not be indented and depth should start at 0
  return (
    <>
      <InfoTreeAttributeNode
        attributes={props.attribute.attributes}
        depth={0}
      />
      {props.attribute.subGroups.map((subGroup) => {
        return (
          <InfoTreeSubNode
            group={subGroup}
            depth={0}
            key={subGroup.name + "_" + 0}
          />)
      })}
    </>
  );
}


/**
 * This component is a recursive element that renders the subgroups and its attributes recursively.
 *
 * @param {AttributeGroup} group The attribute group to render.
 * @param {number} depth The current depth to this group tree.
 */
function InfoTreeSubNode(props: Readonly<InfoTreeSubNodeProps>) {
  const group: AttributeGroup = props.group;
  const depth: number = props.depth;
  // Size of left hand indentation
  const spacing: string = depth * 0.5 + "rem";
  // State for managing collapse and expansion
  const [isCollapsed, setIsCollapsed] = useState<boolean>(group.isCollapsed);
  const toggleExpansion = () => {
    setIsCollapsed(!isCollapsed);
  };
  const collapsedIcon: string = isCollapsed ? "keyboard_arrow_down" : "keyboard_arrow_up";

  // For root element
  if (depth === 0) {
    return (
      <div className={styles.treeEntry}>
        <HeaderField
          name={group.name}
          spacing={spacing}
          isCollapsed={isCollapsed}
          toggleExpansion={toggleExpansion}
        />

        {/* Elements */}
        {!isCollapsed && (<>
          <InfoTreeAttributeNode
            attributes={group.attributes}
            depth={depth + 1}
          />
          {group.subGroups.map((subGroup) => {
            return (<InfoTreeSubNode
              group={subGroup}
              depth={depth + 1}
              key={subGroup.name + "_" + (depth + 1)}
            />)
          })} </>
        )}
      </div>
    );
  } else {
    // For non-root elements
    return (
      <div className={styles.treeEntry}>
        <div className={styles.treeEntryHeader} style={{ paddingLeft: spacing }} onClick={toggleExpansion}>
          {/* Header Name */}
          <div className={styles.treeEntryHeaderName}>
            {group.name}
          </div>
          {/* Expand/collapse icon */}
          <MaterialIconButton
            iconName={collapsedIcon}
            className={iconStyles["push-right"]}
          />
        </div>
        {/* Elements */}
        {!isCollapsed && (<>
          <InfoTreeAttributeNode
            attributes={group.attributes}
            depth={depth + 1}
          />
          {group.subGroups.map((subGroup) => {
            return (
              <InfoTreeSubNode
                group={subGroup}
                depth={depth + 1}
                key={subGroup.name + "_" + (depth + 1)}
              />)
          })}
        </>
        )}
      </div>
    );
  }
}

/**
 * This component renders the attribute for each entry.
 *
 * @param {Attribute[]} attributes The list of attributes that should be rendered.
 * @param {number} depth The current depth to this group tree.
 */
function InfoTreeAttributeNode(props: Readonly<InfoTreeAttributeNodeProps>) {
  const elements: React.ReactElement[] = [];
  const spacing: string = props.depth * 0.5 + "rem";

  props.attributes.map((attribute) => {
    elements.push(
      <p className={styles.treeAttributeEntry} style={{ paddingLeft: spacing }} key={attribute.name + "_" + props.depth}>
        {/* Attribute: Value */}
        <span className={styles.treeAttributeKey}>{attribute.name}:&nbsp;</span>
        {typeof attribute.value === "string" && attribute.value.startsWith("<") ?
          (<div dangerouslySetInnerHTML={{ __html: attribute.value }} />) :
          (<span className={styles.treeAttributeValue}>
            {attribute.value}&nbsp;{attribute.unit}
          </span>)}
      </p>
    );
  });
  return elements;
}