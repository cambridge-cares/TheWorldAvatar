package com.cmclinnovations.agent.service.core;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayDeque;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.stereotype.Service;

import com.cmclinnovations.agent.model.SparqlBinding;

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
   * Get current date in YYYY-MM-DD format.
   */
  public String getCurrentDate() {
    // Define the date format
    return LocalDate.now().format(this.formatter);
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

  /**
   * Retrieve the weekly interval from the recurrence input.
   * 
   * @param recurrence The recurrence interval in P*D format, eg P7D, P14D.
   */
  public int getWeeklyInterval(String recurrence) {
    // Regex to extract numbers from a string
    Pattern pattern = Pattern.compile("P(\\d+)D");
    Matcher matcher = pattern.matcher(recurrence);

    if (matcher.matches()) {
      int number = Integer.parseInt(matcher.group(1));
      return number / 7;
    } else {
      throw new IllegalArgumentException("Input format is incorrect: " + recurrence);
    }
  }

  /**
   * Retrieve dates of occurrences within the period based on the specified
   * interval.
   * 
   * @param startDate    The start date in YYYY-MM-DD format.
   * @param endDateInput The end date in YYYY-MM-DD format.
   * @param interval     The interval of occurrences in days.
   */
  public Queue<String> getOccurrenceDates(String startDate, String endDateInput, int interval) {
    Queue<String> occurrenceDates = new ArrayDeque<>();
    LocalDate currentDate = this.parseDate(startDate);
    LocalDate endDate = this.parseDate(endDateInput);
    // Loop to calculate all occurrence dates
    while (!currentDate.isAfter(endDate)) {
      String currentDateString = currentDate.format(this.formatter);
      occurrenceDates.offer(currentDateString);
      currentDate = currentDate.plusDays(interval); // Increment by the interval
    }
    return occurrenceDates;
  }

  /**
   * Retrieve dates of occurrences within the period based on the days of the week
   * scheduled and the weekly interval.
   * 
   * @param startDateInput The start date in YYYY-MM-DD format.
   * @param endDateInput   The end date in YYYY-MM-DD format.
   * @param bindings       The result set containing the days of week.
   * @param weekInterval   The interval of occurrences in weeks. 1: occur every
   *                       week, 2: occur every 2 weeks
   */
  public Queue<String> getOccurrenceDates(String startDateInput, String endDateInput, SparqlBinding bindings,
      int weekInterval) {
    Queue<String> occurrenceDates = new ArrayDeque<>();
    LocalDate startDate = this.parseDate(startDateInput);
    LocalDate endDate = this.parseDate(endDateInput);
    LocalDate currentDate = this.parseDate(startDateInput);
    // Retrieve scheduled days of week for occurrence
    Set<DayOfWeek> daysOfWeek = this.getScheduledDaysOfWeek(bindings);

    // Loop to calculate all occurrence dates
    while (!currentDate.isAfter(endDate)) {
      // For each week, check all specified days of the week
      for (DayOfWeek dayOfWeek : daysOfWeek) {
        LocalDate occurrenceDate = currentDate.with(dayOfWeek);

        // Ensure the calculated date is within the range [startDate, endDate]
        if (!occurrenceDate.isBefore(startDate) && !occurrenceDate.isAfter(endDate)) {
          String occurrenceDateString = occurrenceDate.format(this.formatter);
          occurrenceDates.offer(occurrenceDateString);
        }
      }
      currentDate = currentDate.plusWeeks(weekInterval); // Increment by the interval
    }
    return occurrenceDates;
  }

  /**
   * Retrieve the scheduled days of the week from the SPARQL binding results.
   * 
   * @param bindings The results for retrieval.
   */
  private Set<DayOfWeek> getScheduledDaysOfWeek(SparqlBinding bindings) {
    Map<String, DayOfWeek> dayMapping = Map.of(
        "monday", DayOfWeek.MONDAY,
        "tuesday", DayOfWeek.TUESDAY,
        "wednesday", DayOfWeek.WEDNESDAY,
        "thursday", DayOfWeek.THURSDAY,
        "friday", DayOfWeek.FRIDAY,
        "saturday", DayOfWeek.SATURDAY,
        "sunday", DayOfWeek.SUNDAY);
    Set<DayOfWeek> daysOfWeek = new HashSet<>();
    for (Map.Entry<String, DayOfWeek> entry : dayMapping.entrySet()) {
      if (!bindings.getFieldValue(entry.getKey()).isEmpty()) {
        daysOfWeek.add(entry.getValue());
      }
    }
    return daysOfWeek;
  }
}
