export const Modules: {
  [key: string]: string;
} = {
  MAP: "map",
  DASHBOARD: "dashboard",
  REGISTRY: "registry",
  HELP:"help",
};

export const Paths: {
  [key: string]: string;
} = {
  HOME: "/",
  MAP: "/map",
  DASHBOARD: "/analytics",
  REGISTRY: "/view",
  REGISTRY_ADD: "/add",
  REGISTRY_EDIT: "/edit",
  REGISTRY_DELETE: "/delete",
  HELP: "/help",
};

// Default routes
export const Routes: {
  [key: string]: string;
} = {
  HOME: Paths.HOME,
  MAP: Paths.MAP,
  DASHBOARD: Paths.DASHBOARD,
  REGISTRY: Paths.REGISTRY,
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
  HELP:"Help",
};