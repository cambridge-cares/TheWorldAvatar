package com.example.notsensorlogger2;

public class SensorItem {
    private String name;
    private int iconResourceId;
    private boolean isToggled;
    private boolean isEnabled;
    private AbstractSensorHandler handler; // Reference to the sensor handler

    // Constructor
    public SensorItem(String name, int iconResourceId, boolean isToggled, AbstractSensorHandler handler) {
        this.name = name;
        this.iconResourceId = iconResourceId;
        this.isToggled = isToggled;
        this.handler = handler; // Set the sensor handler
        this.isEnabled = false;  // MIGHT CHANGE CHECK LATER
    }

    public String getSensorName() {
        return name;
    }

    public int getSensorIconResId() {
        return iconResourceId;
    }

    public boolean isToggled() {
        return isToggled;
    }

    public void setToggled(boolean toggled) {
        isToggled = toggled;
    }

    public boolean isEnabled() {
        return isEnabled;
    }

    public void setEnabled(boolean enabled) {
        isEnabled = enabled;
    }
    // Getter for the handler
    public AbstractSensorHandler getHandler() {
        return handler;
    }

    // Setter for the handler
    public void setHandler(AbstractSensorHandler handler) {
        this.handler = handler;
    }
}
