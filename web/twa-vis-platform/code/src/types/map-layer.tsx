export type MapLayerGroup = {
  name: string;
  address: string;
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