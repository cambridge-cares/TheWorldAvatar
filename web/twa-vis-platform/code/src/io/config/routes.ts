const ASSET_PREFIX = process.env.ASSET_PREFIX ?? "";

export const Modules: {
  [key: string]: string;
} = {
  MAP: "map",
  DASHBOARD: "dashboard",
  REGISTRY: "registry",
  HELP: "help",
};

export const Apis: {
  [key: string]: string;
} = {
  MAP_SETTINGS: `${ASSET_PREFIX}/api/map/settings`,
};

export const Paths: {
  [key: string]: string;
} = {
  HOME: `${ASSET_PREFIX}/`,
  MAP: `${ASSET_PREFIX}/map`,
  DASHBOARD: `${ASSET_PREFIX}/analytics`,
  REGISTRY: `${ASSET_PREFIX}/view`,
  REGISTRY_PENDING: `${ASSET_PREFIX}/registry/pending`,
  REGISTRY_ACTIVE: `${ASSET_PREFIX}/registry/active`,
  REGISTRY_ARCHIVE: `${ASSET_PREFIX}/registry/archive`,
  REGISTRY_ADD: `${ASSET_PREFIX}/add`,
  REGISTRY_EDIT: `${ASSET_PREFIX}/edit`,
  REGISTRY_DELETE: `${ASSET_PREFIX}/delete`,
  HELP: `${ASSET_PREFIX}/help`,
};

// Default routes
export const Routes: {
  [key: string]: string;
} = {
  HOME: Paths.HOME,
  MAP: Paths.MAP,
  DASHBOARD: Paths.DASHBOARD,
  REGISTRY: Paths.REGISTRY,
  REGISTRY_PENDING: Paths.REGISTRY_PENDING,
  REGISTRY_ACTIVE: Paths.REGISTRY_ACTIVE,
  REGISTRY_ARCHIVE: Paths.REGISTRY_ARCHIVE,
  REGISTRY_ADD: Paths.REGISTRY_ADD,
  REGISTRY_EDIT: Paths.REGISTRY_EDIT,
  REGISTRY_DELETE: Paths.REGISTRY_DELETE,
  HELP: Paths.HELP,
};

export const PageTitles: {
  [key: string]: string;
} = {
  MAP: "Explore",
  DASHBOARD: "Analytics",
  REGISTRY: "Registry",
  HELP: "Help",
};
