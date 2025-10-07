package uk.ac.cam.cares.jps.user.setting.sensor;


import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;

/**
 * Represents an individual sensor item with its associated state and metadata.
 * This class encapsulates the sensor's name, description, handler, and its toggle state.
 */
public class SensorItem {
    private final String sensorName;
    private boolean isToggled;
    private final Integer sensorDescription;
    private SensorType sensorType;
    private boolean isToggleEnabled;

    /**
     * Constructs a new SensorItem.
     *
     * @param sensorName The name of the sensor.
     * @param sensorDescription A brief description of the sensor's functionality.
     * @param sensorType The type of sensor
     */
    public SensorItem(String sensorName, Integer sensorDescription, SensorType sensorType) {
        this.sensorName = sensorName;
        this.sensorDescription = sensorDescription;
        this.sensorType = sensorType;
        this.isToggled = false;
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
    public Integer getSensorDescription() {
        return sensorDescription;
    }

    public SensorType getSensorType() {
        return this.sensorType;
    }
}
