package uk.ac.cam.cares.sensorlogger;


/**
 * Represents an individual sensor item within the application. This class holds sensor details and state,
 * including whether it is currently toggled (active) and enabled (able to be used).
 */
public class SensorItem {
    private String name;
    private int iconResourceId;
    private boolean isToggled;
    private SensorHandler handler;

    /**
     * Constructs a new SensorItem.
     *
     * @param name The name of the sensor.
     * @param iconResourceId The resource ID for the sensor's icon.
     * @param isToggled Indicates whether the sensor is initially toggled on or off.
     * @param handler The SensorHandler responsible for managing sensor data.
     */
    public SensorItem(String name, int iconResourceId, boolean isToggled, SensorHandler handler) {
        this.name = name;
        this.iconResourceId = iconResourceId;
        this.isToggled = isToggled;
        this.handler = handler;
    }

    /**
     * Returns the name of the sensor.
     *
     * @return The sensor's name.
     */
    public String getSensorName() {
        return name;
    }

    /**
     * Returns the resource ID of the sensor's icon.
     *
     * @return The resource ID of the icon.
     */
    public int getSensorIconResId() {
        return iconResourceId;
    }

    /**
     * Checks if the sensor is currently toggled.
     *
     * @return true if the sensor is toggled, false otherwise.
     */
    public boolean isToggled() {
        return isToggled;
    }

    /**
     * Sets the toggled state of the sensor.
     *
     * @param toggled The new toggled state.
     */
    public void setToggled(boolean toggled) {
        isToggled = toggled;
    }

    /**
     * Returns the SensorHandler associated with this sensor.
     *
     * @return The sensor's handler.
     */
    public SensorHandler getHandler() {
        return handler;
    }

}
