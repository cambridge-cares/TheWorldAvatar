package com.cmclinnovations.agent.service;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

import org.springframework.stereotype.Service;

@Service
public class DateTimeService {
  private final DateTimeFormatter formatter;

  /**
   * Constructs a new service with the following dependencies.
   */
  public DateTimeService() {
    this.formatter = DateTimeFormatter.ISO_LOCAL_DATE;
  }

  /**
   * Parses the date input string into a LocalDate object with the specified
   * formatter.
   * 
   * @param date The date input string.
   */
  public LocalDate parseDate(String date) {
    return LocalDate.parse(date, this.formatter);
  }

  /**
   * Retrieve the date as a string in the YYYY-MM-DD format from the timestamp
   * input.
   * 
   * @param timestamp The timestamp input in UNIX seconds.
   */
  public String getDateFromTimestamp(long timestamp) {
    // Convert Unix timestamp (seconds) to LocalDate
    return Instant.ofEpochSecond(timestamp)
        .atZone(ZoneId.systemDefault()) // Adjust to the system default time zone
        .toLocalDate()
        .format(this.formatter);
  }
}
