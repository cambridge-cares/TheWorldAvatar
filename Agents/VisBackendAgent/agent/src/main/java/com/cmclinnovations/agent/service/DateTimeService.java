package com.cmclinnovations.agent.service;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import org.springframework.stereotype.Service;

@Service
public class DateTimeService {
  private final DateTimeFormatter formatter;

  /**
   * Constructs a new service with the following dependencies.
   */
  public DateTimeService() {
    this.formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

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
   * Parses the date input into a String.
   * 
   * @param date The LocalDate input.
   */
  public String parseDateToString(LocalDate date) {
    return date.format(this.formatter);
  }
}
