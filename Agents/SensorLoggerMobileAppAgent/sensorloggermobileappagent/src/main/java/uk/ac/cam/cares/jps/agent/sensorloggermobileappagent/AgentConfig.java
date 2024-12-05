package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import uk.ac.cam.cares.downsampling.Downsampling;

import java.util.ResourceBundle;

public class AgentConfig {
    private final Long accelDSResolution;
    private final Downsampling.Type accelDSType;

    private final Long gravityDSResolution;
    private final Downsampling.Type gravityDSType;

    private final Long magnetometerDSResolution;
    private final Downsampling.Type magnetometerDSType;

    private final Long dbfsDSResolution;
    private final Downsampling.Type dbfsDSType;

    private final Long rbDSResolution;
    private final Downsampling.Type rbDSType;

    private final Long lightValueDSResolution;
    private final Downsampling.Type lightValueDSType;

    private final int timerDelay;
    private final int timerFrequency;

    private final int taskInactiveTime;

    public AgentConfig() {
        ResourceBundle config = ResourceBundle.getBundle("config");

        accelDSResolution = Long.valueOf(config.getString("accelDSResolution"));
        accelDSType = Downsampling.Type.valueOf(config.getString("accelDSType"));

        gravityDSResolution = Long.valueOf(config.getString("gravityDSResolution"));
        gravityDSType = Downsampling.Type.valueOf(config.getString("gravityDSType"));

        magnetometerDSResolution = Long.valueOf(config.getString("magnetometerDSResolution"));
        magnetometerDSType = Downsampling.Type.valueOf(config.getString("magnetometerDSType"));

        dbfsDSResolution = Long.valueOf(config.getString("dbfsDSResolution"));
        dbfsDSType = Downsampling.Type.valueOf(config.getString("dbfsDSType"));

        rbDSResolution = Long.valueOf(config.getString("rbDSResolution"));
        rbDSType = Downsampling.Type.valueOf(config.getString("rbDSType"));

        lightValueDSResolution = Long.valueOf(config.getString("lightValueDSResolution"));
        lightValueDSType = Downsampling.Type.valueOf(config.getString("lightValueDSType"));

        timerDelay = Integer.valueOf(config.getString("timerDelay"));
        timerFrequency = Integer.valueOf(config.getString("timerFrequency"));

        taskInactiveTime = Integer.valueOf(config.getString("taskInactiveTime"));
    }

    public Long getAccelDSResolution() {
        return accelDSResolution;
    }

    public Downsampling.Type getAccelDSType() {
        return accelDSType;
    }

    public Long getGravityDSResolution() {
        return gravityDSResolution;
    }

    public Downsampling.Type getGravityDSType() {
        return gravityDSType;
    }

    public Long getMagnetometerDSResolution() {
        return magnetometerDSResolution;
    }

    public Downsampling.Type getMagnetometerDSType() {
        return magnetometerDSType;
    }

    public Long getDbfsDSResolution() {
        return dbfsDSResolution;
    }

    public Downsampling.Type getDbfsDSType() {
        return dbfsDSType;
    }

    public Long getRbDSResolution() {
        return rbDSResolution;
    }

    public Downsampling.Type getRbDSType() {
        return rbDSType;
    }

    public Long getLightValueDSResolution() {
        return lightValueDSResolution;
    }

    public Downsampling.Type getLightValueDSType() {
        return lightValueDSType;
    }

    public int getTimerDelay() {
        return timerDelay;
    }

    public int getTimerFrequency() {
        return timerFrequency;
    }

    public int getTaskInactiveTime() {
        return taskInactiveTime;
    }
}
