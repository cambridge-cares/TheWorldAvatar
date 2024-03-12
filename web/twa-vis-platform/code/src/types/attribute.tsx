export type AttributeGroup = {
  name: string;
  attributes: Attribute[];
  subGroups?: AttributeGroup[];
  displayOrder?: string[];
  isCollapsed: boolean; // track open/closed state
};

export type Attribute = {
  name: string;
  value: string;
  unit?: string;
};