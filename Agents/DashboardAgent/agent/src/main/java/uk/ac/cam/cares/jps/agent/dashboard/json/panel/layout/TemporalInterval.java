package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import java.util.HashMap;
import java.util.Map;

/**
 * A class storing the specified intervals.
 *
 * @author qhouyee
 */
public class TemporalInterval {
    public static final String DAILY_OVER_WEEK = "Daily over last week";
    public static final String DAILY_OVER_MONTH = "Daily over last month";
    public static final String WEEKLY_OVER_MONTH = "Weekly over last month";
    public static final String MONTHLY = "Monthly";
    private static final String JANUARY = "January";
    private static final String FEBRUARY = "February";
    private static final String MARCH = "March";
    private static final String APRIL = "April";
    private static final String MAY = "May";
    private static final String JUNE = "June";
    private static final String JULY = "July";
    private static final String AUGUST = "August";
    private static final String SEPTEMBER = "September";
    private static final String OCTOBER = "October";
    private static final String NOVEMBER = "November";
    private static final String DECEMBER = "December";
    private static final Map<String, String> MONTH_MAP;

    // Private constructor to prevent instantiation.
    private TemporalInterval() {
    }

    // Initialises the map for the months in a static block
    static {
        MONTH_MAP = new HashMap<>();
        MONTH_MAP.put(JANUARY, "1");
        MONTH_MAP.put(FEBRUARY, "2");
        MONTH_MAP.put(MARCH, "3");
        MONTH_MAP.put(APRIL, "4");
        MONTH_MAP.put(MAY, "5");
        MONTH_MAP.put(JUNE, "6");
        MONTH_MAP.put(JULY, "7");
        MONTH_MAP.put(AUGUST, "8");
        MONTH_MAP.put(SEPTEMBER, "9");
        MONTH_MAP.put(OCTOBER, "10");
        MONTH_MAP.put(NOVEMBER, "11");
        MONTH_MAP.put(DECEMBER, "12");
    }

    public static Map<String, String> getMonthMap() {
        return MONTH_MAP;
    }
}
