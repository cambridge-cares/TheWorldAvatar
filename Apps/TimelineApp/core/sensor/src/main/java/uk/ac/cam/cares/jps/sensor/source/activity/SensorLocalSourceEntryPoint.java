package uk.ac.cam.cares.jps.sensor.source.activity;

import dagger.hilt.EntryPoint;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;

@EntryPoint
@InstallIn(SingletonComponent.class)
public interface SensorLocalSourceEntryPoint {
    SensorLocalSource getSensorLocalSource();
}