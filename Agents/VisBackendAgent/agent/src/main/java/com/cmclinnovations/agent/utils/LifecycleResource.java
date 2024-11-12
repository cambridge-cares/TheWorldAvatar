package com.cmclinnovations.agent.utils;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.cmclinnovations.agent.model.type.LifecycleEventType;

public class LifecycleResource {
  public static final String LIFECYCLE_RESOURCE = "lifecycle";
  public static final String SCHEDULE_RESOURCE = "schedule";

  public static final String CONTRACT_KEY = "contract";
  public static final String CURRENT_DATE_KEY = "current date";
  public static final String EVENT_KEY = "event";

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

  /**
   * Retrieve the event class associated with the event type.
   */
  public static String getEventClass(LifecycleEventType eventType) {
    switch (eventType) {
      case LifecycleEventType.SERVICE_EXECUTION:
        return "https://www.theworldavatar.com/kg/ontoservice/ServiceDeliveryEvent";
      default:
        throw new IllegalArgumentException("Invalid event type!");
    }
  }
}