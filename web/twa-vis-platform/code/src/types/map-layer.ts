export type MapLayerGroup = {
  name: string;
  address: string; // Marks the current position of the group in the hierarchy. 0 is first group, while 0.0 is first subgroup
  subGroups?: MapLayerGroup[];
  icon?: string;
  layers?: MapLayer[];
  showChildren?: boolean; // track open/closed state
};

export type MapLayer = {
  name: string;
  address: string;
  ids: string;
  icon?: string;
  isVisible: boolean; // track visibility
};