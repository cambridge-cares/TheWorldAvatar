package com.cmclinnovations.agent.utils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.fasterxml.jackson.databind.JsonNode;

public class LifecycleResource {
  public static final String LIFECYCLE_RESOURCE = "lifecycle";

  public static final String CONTRACT_KEY = "contract";
  public static final String CURRENT_DATE_KEY = "current date";

  // Private constructor to prevent instantiation
  private LifecycleResource() {
    throw new UnsupportedOperationException("This class cannot be instantiated!");
  }

  /**
   * Get current date in YYYY-MM-DD format.
   */
  public static String getCurrentDate() {
    // Define the date format
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    return LocalDate.now().format(formatter);
  }
}