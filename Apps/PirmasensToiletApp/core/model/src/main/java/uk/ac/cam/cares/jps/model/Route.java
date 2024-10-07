package uk.ac.cam.cares.jps.model;

public class Route {

    private final double WALKING_SPEED_KM_PER_H = 4.54;
    private final double CYCLING_SPEED_KM_PER_H = 20;

    String geojsonString;
    double costInMeter;

    public Route(String route, double costInMeter) {
        geojsonString = route;
        this.costInMeter = costInMeter;
    }

    public String getGeojsonString() {
        return geojsonString;
    }

    public double getCostInMeter() {
        return costInMeter;
    }

    public String getCostWithUnit() {
        if (costInMeter == 0) {
            return "-";
        } else if (costInMeter > 1000) {
            return String.format("%.2f km", costInMeter/1000);
        } else {
            return String.format("%.0f m", costInMeter);
        }
    }

    public String getWalkingTime() {
        return roundUnit(costInMeter/1000/WALKING_SPEED_KM_PER_H);
    }

    public String getDrivingTime() {
        // todo: not implemented yet
        return "NOT IMPLEMENTED";
    }

    public String getCyclingTime() {
        return roundUnit(costInMeter/1000/CYCLING_SPEED_KM_PER_H);
    }

    private String roundUnit(double timeInHour) {
        if (timeInHour == 0) {
            return "-";
        }
        else if (timeInHour < 1) {
            return String.format("%.0f min", Math.ceil(timeInHour * 60));
        } else {
            return String.format("%.0f hr %.0f min", timeInHour, (timeInHour -  (int) timeInHour) * 60);
        }
    }
}
