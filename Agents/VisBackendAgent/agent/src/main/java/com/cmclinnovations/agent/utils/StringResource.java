package com.cmclinnovations.agent.utils;

public class StringResource {
  // Public constants
  private static final String RESOURCE_DIR = "file:/usr/local/tomcat/resources/";
  public static final String CONFIG_DIR = RESOURCE_DIR + "config/";

  // Private constructor to prevent instantiation
  private StringResource() {
    throw new AssertionError("Cannot instantiate this class");
  }
}
