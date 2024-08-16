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