package uk.ac.cam.cares.jps.user;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandler;


/**
 * Represents an individual sensor item with its associated state and metadata.
 * This class encapsulates the sensor's name, description, handler, and its toggle state.
 */
public class SensorItem {
    private String sensorName;
    private boolean isToggled;
    private SensorHandler sensorHandler;
    private String sensorDescription;
    private boolean isToggleEnabled;

    /**
     * Constructs a new SensorItem.
     *
     * @param sensorName The name of the sensor.
     * @param sensorDescription A brief description of the sensor's functionality.
     * @param sensorHandler The handler responsible for managing the sensor's data collection.
     */
    public SensorItem(String sensorName, String sensorDescription, SensorHandler sensorHandler) {
        this.sensorName = sensorName;
        this.sensorHandler = sensorHandler;
        this.sensorDescription = sensorDescription;
        this.isToggled = sensorHandler.isRunning();
        this.isToggleEnabled = true;
    }

    /**
     * Returns the name of the sensor.
     *
     * @return The sensor name.
     */
    public String getSensorName() {
        return sensorName;
    }

    /**
     * Returns the current toggle state of the sensor.
     *
     * @return true if the sensor is toggled on; false otherwise.
     */
    public boolean isToggled() {
        return isToggled;
    }

    /**
     * Sets the toggle state of the sensor.
     *
     * @param toggled true to toggle the sensor on; false to toggle it off.
     */
    public void setToggled(boolean toggled) {
        isToggled = toggled;
    }

    /**
     * Returns the SensorHandler associated with this sensor.
     *
     * @return The SensorHandler managing this sensor.
     */
    public SensorHandler getSensorHandler() {
        return sensorHandler;
    }

    /**
     * Enables or disables the toggle functionality for this sensor.
     *
     * @param enabled true to enable toggling; false to disable it.
     */
    public void setToggleEnabled(boolean enabled) {
        isToggleEnabled = enabled;
    }

    /**
     * Returns whether the toggle functionality is enabled for this sensor.
     *
     * @return true if the toggle is enabled; false otherwise.
     */
    public boolean isToggleEnabled() {
        return isToggleEnabled;
    }

    /**
     * Returns the description of the sensor.
     *
     * @return A brief description of the sensor.
     */
    public String getSensorDescription() {
        return sensorDescription;
    }
}
