

import styles from '../info-tree.module.css';

import { Icon } from '@mui/material';
import { useState } from 'react';

import { Attribute, AttributeGroup } from 'types/attribute';

// type definition for incoming properties
type InfoTreeNodeProps = {
  attribute: AttributeGroup;
};

type InfoTreeSubNodeProps = {
  group: AttributeGroup;
  depth: number;
};

type InfoTreeAttributeNodeProps = {
  attributes: Attribute[];
  depth: number;
};

/**
 * This component renders the Info Tree based on the input data.
 *
 * @param {AttributeGroup} attribute The attribute group to render.
 */
export default function InfoTreeNode(props: InfoTreeNodeProps) {
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
function InfoTreeSubNode(props: InfoTreeSubNodeProps) {
  const group: AttributeGroup = props.group;
  const depth: number = props.depth;
  // Size of left hand indentation
  const spacing: string = depth * 0.5 + "rem";
  // State for managing collapse and expansion
  const [isCollapsed, setIsCollapsed] = useState<boolean>(group.isCollapsed);
  const toggleExpansion = () => {
    setIsCollapsed(!isCollapsed);
  };

  // For root element
  if (depth === 0) {
    return (
      <div className={styles.treeEntry} key={group.name}>
        <div style={{ paddingLeft: spacing }} className={styles.treeEntryHeader} onClick={toggleExpansion}>
          {/* Header Name */}
          <div className={styles.treeHeaderName}>
            {group.name}
          </div>

          {/* Expand/collapse icon */}
          <div className={styles.icon}>
            <Icon className="material-symbols-outlined">keyboard_arrow_up</Icon>
          </div>
        </div>

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
            />)
          })} </>
        )}
      </div>
    );
  } else {
    // For non-root elements
    return (
      <div className={styles.treeEntry} key={group.name}>
        <div className={styles.treeEntryNonPrimaryHeader} style={{ paddingLeft: spacing }} onClick={toggleExpansion}>
          {/* Header Name */}
          <div className={styles.treeEntryNonPrimaryHeaderName}>
            {group.name}
          </div>
          {/* Expand/collapse icon */}
          <div className={styles.icon}>
            <Icon className="material-symbols-outlined">keyboard_arrow_up</Icon>
          </div>
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
function InfoTreeAttributeNode(props: InfoTreeAttributeNodeProps) {
  const elements: React.ReactElement[] = [];
  const spacing: string = props.depth * 0.5 + "rem";

  props.attributes.map((attribute) => {
    elements.push(
      <p className={styles.treeAttributeEntry} style={{ paddingLeft: spacing }} key={attribute.name}>
        {/* Attribute: Value */}
        <span className={styles.treeAttributeKey}>{attribute.name}:&nbsp;</span>
        <span className={styles.treeAttributeValue}>
          {attribute.value}&nbsp;{attribute.unit}
        </span>
      </p>
    );
  });
  return elements;
}