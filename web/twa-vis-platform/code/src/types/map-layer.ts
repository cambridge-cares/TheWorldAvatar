export type MapLayerGroup = {
  name: string;
  address: string; // Marks the current position of the group in the hierarchy. 0 is first group, while 0.0 is first subgroup
  subGroups?: MapLayerGroup[];
  icon?: string;
  layers?: MapLayer[];
  stack?: string; // Optional stack endpoint
  search?: string; // Optional search resource identifier
  showChildren?: boolean; // track open/closed state
  groupings: string[]; // unique array of groupings
};

export type MapLayer = {
  name: string;
  address: string;
  ids: string;
  icon?: string;
  grouping?: string; // Map layer grouping if available
  isVisible: boolean; // track visibility
};