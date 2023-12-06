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
        long tempFahrenheit = Math.round((temperature * 1.8) + 32);
        // there are only 3 spaces for temperature
        if (tempFahrenheit < -100) {
            throw new RuntimeException("Calculated temperature is less than -100: " + tempFahrenheit);
        }
        return tempFahrenheit;
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