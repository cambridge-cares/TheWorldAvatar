package uk.ac.cam.cares.jps.timeline.ui.datepicker;

import java.util.Objects;

public class YearMonthCompositeKey {
    private final int year;
    private final int month;

    public YearMonthCompositeKey(int year, int month) {
        this.year = year;
        this.month = month;
    }

    public int getYear() {
        return year;
    }

    public int getMonth() {
        return month;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        YearMonthCompositeKey that = (YearMonthCompositeKey) o;
        return year == that.year && month == that.month;
    }

    @Override
    public int hashCode() {
        return Objects.hash(year, month);
    }

    @Override
    public String toString() {
        return "CompositeKey{" +
                "year=" + year +
                ", month=" + month +
                '}';
    }
}
