package com.cmclinnovations.aermod.objects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class WeatherData {
    public List<String> timeStamps;
    // comments on the right indicates unit stored in KG
    private List<Double> temperature; // celcius
    private List<Double> humidity; // fraction
    private List<Double> windSpeed; // m/s
    private List<Double> windDirection; // degrees
    private List<Double> cloudCover; // %
    private int numberTimeSteps;

    public void setTemperatureInCelcius(List<Double> temperature) {
        this.temperature = temperature;
    }

    public void setHumidityInPercentage(List<Double> humidity) {
        this.humidity = humidity;
    }

    public void setWindSpeedInMetreSecond(List<Double> windSpeed) {
        this.windSpeed = windSpeed;
    }

    public void setWindDirectionInDegrees(List<Double> windDirection) {
        this.windDirection = windDirection;
    }

    public void setCloudCoverInPercentage(List<Double> cloudCover) {
        this.cloudCover = cloudCover;
    }

    public void setNumberTimesteps(int n) {
        this.numberTimeSteps = n;
    }

    public List<Long> getTemperatureInFahrenheit() {

        Double temp = Collections.min(temperature);
        if (temp < 0) {
            throw new RuntimeException("Temperatures below zero degree Celsius are not supported yet");
        }
        List<Long> res = temperature.stream().map(x -> Math.round((x * 1.8) + 32)).collect(Collectors.toList());
        return res;
    }

    public List<Long> getWindSpeedInKnots() {
        // Conversion factor obtained from Google
        return windSpeed.stream().map(x -> Math.round(x * 1.943844)).collect(Collectors.toList());
    }

    public List<Long> getHumidityAsPercentage() {
        return humidity.stream().map(x -> Math.round(x)).collect(Collectors.toList());
    }

    public List<Long> getWindDirectionInTensOfDegrees() {
        return windDirection.stream().map(x -> Math.round(x / 10)).collect(Collectors.toList());
    }

    /**
     * between 0-9
     * 
     * @return
     */
    public List<Long> getCloudCoverAsInteger() {

        List<Long> res = new ArrayList<>();
        for (int i = 0; i < cloudCover.size(); i++) {
            double clcvr = cloudCover.get(i);
            long roundedValue = Math.round(clcvr / 10);
            if (roundedValue == 10) {
                roundedValue = 9;
            }
            if (roundedValue > 10) {
                throw new RuntimeException("Invalid cloud cover value " + cloudCover);
            }
            res.add(roundedValue);
        }

        return res;
    }

    public int getNumberTimesteps() {
        return numberTimeSteps;
    }

}
