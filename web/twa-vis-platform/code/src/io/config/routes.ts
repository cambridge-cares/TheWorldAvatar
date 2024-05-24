export const PathNames: {
  [key: string]: string;
} = {
  HOME: "",
  MAP: "explore",
  DASHBOARD: "analytics",
  HELP: "help",
};

// Default routes
export const Routes: {
  [key: string]: string;
} = {
  HOME: "/" + PathNames.HOME,
  MAP: "/" + PathNames.MAP,
  DASHBOARD: "/" + PathNames.DASHBOARD,
  HELP: "/" + PathNames.HELP,
};