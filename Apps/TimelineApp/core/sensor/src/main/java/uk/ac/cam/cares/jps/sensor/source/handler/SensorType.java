package uk.ac.cam.cares.jps.sensor.source.handler;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;



/**
 * Enum representing different types of sensors available in the application.
 * This enum implements the Parcelable interface, allowing SensorType instances to be passed between Android components.
 */
public enum SensorType implements Parcelable {
    ACCELEROMETER,
    GYROSCOPE,
    MAGNETOMETER,
    LIGHT,
    HUMIDITY,
    PRESSURE,
    GRAVITY,
    LOCATION,
    SOUND,
    ACTIVITY;

    /**
     * Describes the contents of the parcelable object.
     *
     * @return an integer bitmask indicating the set of special object types marshalled by the Parcelable.
     */
    @Override
    public int describeContents() {
        return 0;
    }

    /**
     * Writes the SensorType to a Parcel.
     *
     * @param parcel The Parcel in which the object should be written.
     * @param flags  Additional flags about how the object should be written. May be 0 or Parcelable.PARCELABLE_WRITE_RETURN_VALUE.
     */
    @Override
    public void writeToParcel(@NonNull Parcel parcel, int flags) {
        parcel.writeString(this.name());
    }

    /**
     * Creator object for creating instances of SensorType from a Parcel.
     */
    public static final Parcelable.Creator<SensorType> CREATOR = new Parcelable.Creator<SensorType>() {

        /**
         * Creates a new instance of the SensorType enum from the given Parcel.
         *
         * @param in The Parcel containing the serialized SensorType.
         * @return A new instance of SensorType corresponding to the serialized data.
         */
        @Override
        public SensorType createFromParcel(Parcel in) {
            return SensorType.valueOf(in.readString());
        }

        /**
         * Creates a new array of the SensorType enum.
         *
         * @param size The size of the array to create.
         * @return An array of the SensorType enum, with each element initially null.
         */
        @Override
        public SensorType[] newArray(int size) {
            return new SensorType[size];
        }
    };

    //todo: need to standardize the sensorName and sensorType.name()
    public final String getSensorName() {
        return switch (this) {
            case LOCATION -> "location";
            case ACCELEROMETER -> "accelerometer";
            case SOUND -> "microphone";
            case GRAVITY -> "gravity";
            case GYROSCOPE -> "gyroscope";
            case LIGHT -> "light";
            case MAGNETOMETER -> "magnetometer";
            case PRESSURE -> "pressure";
            case HUMIDITY -> "humidity";
            case ACTIVITY -> "activity";
            default -> this.toString().toLowerCase();
        };
    }

}
