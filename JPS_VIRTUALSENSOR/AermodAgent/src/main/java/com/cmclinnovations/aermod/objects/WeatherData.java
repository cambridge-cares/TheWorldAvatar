package com.cmclinnovations.aermod.objects;

public class WeatherData {
    // comments on the right indicates unit stored in KG
    private double temperature; // celcius
    private double humidity; // fraction
    private double windSpeed; // m/s
    private double windDirection; // degrees
    private double cloudCover; // %

    public void setTemperatureInCelcius(double temperature) {
        this.temperature = temperature;
    }

    public void setHumidityInPercentage(double humidity) {
        this.humidity = humidity;
    }

    public void setWindSpeedInMetreSecond(double windSpeed) {
        this.windSpeed = windSpeed;
    }

    public void setWindDirectionInDegrees(double windDirection) {
        this.windDirection = windDirection;
    }

    public void setCloudCoverInPercentage(double cloudCover) {
        this.cloudCover = cloudCover;
    }

    public long getTemperatureInFahrenheit() {
        if (temperature < 0) {
            throw new RuntimeException("Temperatures below zero degree Celsius are not supported yet");
        }
        return Math.round((temperature * 1.8) + 32);
    }

    public long getWindSpeedInKnots() {
        return Math.round(windSpeed * 1.943844); // factor given by Google
    }

    public long getHumidityAsPercentage() {
        return Math.round(humidity);
    }

    public long getWindDirectionInTensOfDegrees() {
        return Math.round(windDirection / 10);
    }

    /**
     * between 0-9
     * 
     * @return
     */
    public long getCloudCoverAsInteger() {
        long roundedValue = Math.round(cloudCover / 10);
        if (roundedValue == 10) {
            roundedValue = 9;
        }
        if (roundedValue > 10) {
            throw new RuntimeException("Invalid cloud cover value " + cloudCover);
        }
        return roundedValue;
    }
}