/**
 * Configuration for "jest" testing framework.
 */
const nextJest = require("next/jest");

const createJestConfig = nextJest({
  dir: "./",
});

const customJestConfig = {
  moduleDirectories: ["node_modules", "<rootDir>/"],
  bail: false
};

module.exports = createJestConfig(customJestConfig);