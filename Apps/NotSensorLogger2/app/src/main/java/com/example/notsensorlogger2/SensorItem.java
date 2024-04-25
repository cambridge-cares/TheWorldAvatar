// SensorItem.java
package com.example.notsensorlogger2;

public class SensorItem {
    private String name;
    private int iconResourceId;
    private boolean isToggled;
    private boolean isEnabled;
    private SensorHandler handler; // Using the existing SensorHandler interface

    // Constructor
    public SensorItem(String name, int iconResourceId, boolean isToggled, SensorHandler handler) {
        this.name = name;
        this.iconResourceId = iconResourceId;
        this.isToggled = isToggled;
        this.handler = handler;
        this.isEnabled = false;  // Might change, check later
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

    public SensorHandler getHandler() {
        return handler;
    }

    public void setHandler(SensorHandler handler) {
        this.handler = handler;
    }
}
