import styles from './info-tree.module.css';
import parentStyles from '../floating-panel.module.css';

import React, { useEffect, useState } from 'react';
import { useDispatch } from 'react-redux';

import { Attribute, AttributeGroup } from 'types/attribute';
import HeaderField from 'ui/text/header';
import { Assets } from 'io/config/assets';
import { setHasExistingData } from 'state/floating-panel-slice';
import { setIri, setStack } from 'state/map-feature-slice';

// type definition for incoming properties
interface AttributeRootProps {
  attribute: AttributeGroup;
  scrollRef: React.MutableRefObject<HTMLDivElement>;
  setScrollPosition: React.Dispatch<React.SetStateAction<number>>;
}

interface AttributeNodeProps {
  group: AttributeGroup;
  depth: number;
  scrollRef: React.MutableRefObject<HTMLDivElement>;
  setScrollPosition: React.Dispatch<React.SetStateAction<number>>;
}

interface AttributeTextNodeProps {
  attributes: Attribute[];
  depth: number;
}

/**
 * This component renders the input attributes as a tree starting from this root.
 *
 * @param {AttributeGroup} attribute The attribute group to render.
 */
export default function AttributeRoot(props: Readonly<AttributeRootProps>) {
  // This component is separate from the SubNode as the root node should not be indented and depth should start at 0
  return (
    <>
      <AttributeTextNode
        attributes={props.attribute.attributes}
        depth={0}
      />
      {props.attribute.subGroups.map((subGroup) => {
        return (
          <AttributeNode
            group={subGroup}
            depth={0}
            key={subGroup.name + "_" + 0}
            scrollRef={props.scrollRef}
            setScrollPosition={props.setScrollPosition}
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
function AttributeNode(props: Readonly<AttributeNodeProps>) {
  const dispatch = useDispatch();
  const group: AttributeGroup = props.group;
  const depth: number = props.depth;
  // Size of left hand indentation
  const spacing: string = depth * 0.5 + "rem";
  // State for managing collapse and expansion
  const [isCollapsed, setIsCollapsed] = useState<boolean>(group.isCollapsed);
  const [isSubqueryLoading, setIsSubqueryLoading] = useState<boolean>(false);
  const collapsedIcon: string = isCollapsed ? "keyboard_arrow_right" : "keyboard_arrow_down";
  const displayIcon: string = group.subQueryIri ? Assets.SUBQUERY : collapsedIcon;

  const toggleExpansion = () => {
    if (group.subQueryIri) {
      setIsSubqueryLoading(true);
      props.setScrollPosition(props.scrollRef.current.scrollTop);

      dispatch(setHasExistingData(true));
      dispatch(setIri(group.subQueryIri));
      // Only update the selected stack if it is required for the subquery
      if (group.subQueryStack) {
        dispatch(setStack(group.subQueryStack));
      }
      setIsCollapsed(!isCollapsed)
    }
    else {
      setIsCollapsed(!isCollapsed);
    }
  };

  useEffect(() => {
    // This effect runs to end the loading indicator when the subquery have been completed
    // The effects occurs when the group has been updated and it was previously loading the subquery
    if (group.attributes && isSubqueryLoading) {
      setIsSubqueryLoading(false);
    }
  }, [group]);

  // Header element differs for root element in styling
  const headerElement = depth === 0 ?
    <HeaderField
      name={group.name}
      icon={displayIcon}
      containerStyle={parentStyles.treeHeader}
      headerNameStyle={parentStyles.treeHeaderName}
      isLoading={isSubqueryLoading}
      spacing={spacing}
      toggleExpansion={toggleExpansion}
    /> : <HeaderField
      name={group.name}
      icon={displayIcon}
      containerStyle={styles.treeEntrySubHeader}
      headerNameStyle={styles.treeEntrySubHeaderName}
      isLoading={isSubqueryLoading}
      spacing={spacing}
      toggleExpansion={toggleExpansion}
    />;

  return (
    <div id="Tree Group">
      {headerElement}

      {/* Elements */}
      {!isCollapsed && (<>
        {/* Do not render IRI display for subqueries if present */}
        {!group.subQueryIri && <AttributeTextNode
          attributes={group.attributes}
          depth={depth + 1}
        />}
        {group.subGroups.map((subGroup) => {
          return (<AttributeNode
            group={subGroup}
            depth={depth + 1}
            key={subGroup.name + "_" + (depth + 1)}
            scrollRef={props.scrollRef}
            setScrollPosition={props.setScrollPosition}
          />)
        })}
      </>
      )}
    </div>
  );
}

/**
 * This component renders the attribute for each entry.
 *
 * @param {Attribute[]} attributes The list of attributes that should be rendered.
 * @param {number} depth The current depth to this group tree.
 */
function AttributeTextNode(props: Readonly<AttributeTextNodeProps>) {
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