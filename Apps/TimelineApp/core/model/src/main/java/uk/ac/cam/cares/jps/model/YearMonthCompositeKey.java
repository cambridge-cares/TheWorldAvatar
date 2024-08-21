package uk.ac.cam.cares.jps.model;

public record YearMonthCompositeKey(int year, int month) {

    @Override
    public String toString() {
        return "CompositeKey{" +
                "year=" + year +
                ", month=" + month +
                '}';
    }
}
