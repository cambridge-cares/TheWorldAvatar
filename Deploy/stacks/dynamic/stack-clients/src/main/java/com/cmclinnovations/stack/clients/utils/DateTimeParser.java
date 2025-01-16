package com.cmclinnovations.stack.clients.utils;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;

public class DateTimeParser {

    private final DateTimeFormatter formatter;

    private final ZoneId timeZoneID;

    public DateTimeParser(String dateTimeFormat, String timeZone) {
        formatter = new DateTimeFormatterBuilder()
                .appendPattern(dateTimeFormat)
                .optionalStart()
                .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1)
                .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                .parseDefaulting(ChronoField.OFFSET_SECONDS, 0)
                .optionalEnd()
                .toFormatter();
        timeZoneID = null != timeZone ? ZoneId.of(timeZone) : null;
    }

    public java.time.ZonedDateTime parse(String dateTimeString) {

        if (null == timeZoneID) {
            return ZonedDateTime.parse(dateTimeString, formatter);
        } else {
            LocalDateTime localDateTime = LocalDateTime.parse(dateTimeString, formatter);
            return localDateTime.atZone(timeZoneID);
        }
    }
 }