export type AttributeGroup = {
  name: string;
  attributes: Attribute[];
  subGroups?: AttributeGroup[];
  displayOrder?: string[];
  isCollapsed: boolean; // track open/closed state
  subQueryIri: string // triggers subqueries to extract more information
};

export type Attribute = {
  name: string;
  value: string;
  unit?: string;
};