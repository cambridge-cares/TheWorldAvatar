export const Modules: {
  [key: string]: string;
} = {
  MAP: "map",
  DASHBOARD: "dashboard",
  REGISTRY: "registry",
  HELP:"help",
};

export const PathNames: {
  [key: string]: string;
} = {
  HOME: "./",
  MAP: "map",
  DASHBOARD: "analytics",
  REGISTRY: "view",
  REGISTRY_ADD: "add",
  REGISTRY_EDIT: "edit",
  REGISTRY_DELETE: "delete",
  SEARCH: "search",
  HELP: "help",
};

// Default routes
export const Routes: {
  [key: string]: string;
} = {
  HOME: PathNames.HOME,
  MAP: PathNames.MAP,
  DASHBOARD: PathNames.DASHBOARD,
  REGISTRY: PathNames.REGISTRY,
  REGISTRY_ADD: PathNames.REGISTRY_ADD,
  REGISTRY_EDIT: PathNames.REGISTRY_EDIT,
  REGISTRY_DELETE: PathNames.REGISTRY_DELETE,
  HELP: PathNames.HELP,
};

export const PageTitles: {
  [key: string]: string;
} = {
  MAP: "Explore",
  DASHBOARD: "Analytics",
  REGISTRY: "Registry",
  HELP:"Help",
};