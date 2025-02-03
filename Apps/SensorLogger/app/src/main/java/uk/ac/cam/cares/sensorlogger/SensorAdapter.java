package uk.ac.cam.cares.sensorlogger;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.switchmaterial.SwitchMaterial;
import java.util.List;

/**
 * Adapter for displaying sensor items in a RecyclerView. Each item represents a sensor
 * and its current state, allowing the user to toggle the sensor's activation state and access more details.
 */
public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {

    private List<SensorItem> sensorsList;
    private OnSensorToggleListener toggleListener;
    private OnSensorDetailsListener detailsListener;

    /**
     * Constructs a SensorAdapter with a dataset of sensors, a toggle listener, and a details listener.
     *
     * @param myDataset The list of sensor items to display.
     * @param toggleListener The listener for handling toggle actions on sensors.
     * @param detailsListener The listener for handling requests for more details about a sensor.
     */
    public SensorAdapter(List<SensorItem> myDataset, OnSensorToggleListener toggleListener, OnSensorDetailsListener detailsListener) {
        sensorsList = myDataset;
        this.toggleListener = toggleListener;
        this.detailsListener = detailsListener;
    }

    /**
     * Creates new views (invoked by the layout manager). Inflates the sensor item layout.
     *
     * @param parent The ViewGroup into which the new View will be added after it is bound to
     *               an adapter position.
     * @param viewType The view type of the new View.
     * @return A new SensorViewHolder that holds the View for each sensor item.
     */
    @Override
    public SensorViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_sensor, parent, false);
        return new SensorViewHolder(v);
    }

    /**
     * Replaces the contents of a view (invoked by the layout manager).
     *
     * @param holder The SensorViewHolder which should be updated to represent the contents
     *               of the item at the given position in the data set.
     * @param position The position of the item within the adapter's data set.
     */
    @Override
    public void onBindViewHolder(SensorViewHolder holder, int position) {
        SensorItem sensorItem = sensorsList.get(position);
        holder.sensorName.setText(sensorItem.getSensorName());
        holder.sensorIcon.setImageResource(sensorItem.getSensorIconResId());

        holder.sensorSwitch.setOnCheckedChangeListener(null);
        holder.sensorSwitch.setChecked(sensorItem.isToggled());

        holder.sensorSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            toggleListener.onSensorToggled(sensorItem, isChecked);
        });

        holder.sensorDetailsButton.setOnClickListener(view -> {
            if (detailsListener != null) {
                detailsListener.onSensorDetailsRequested(sensorItem);
            }
        });
    }

    /**
     * Returns the total number of items in the data set held by the adapter.
     *
     * @return The total number of sensor items in this adapter.
     */
    @Override
    public int getItemCount() {
        return sensorsList.size();
    }

    /**
     * Interface for handling toggle events on sensors.
     */
    public interface OnSensorToggleListener {
        void onSensorToggled(SensorItem sensor, boolean isChecked);
    }

    /**
     * Interface for handling requests for more details on sensors.
     */
    public interface OnSensorDetailsListener {
        void onSensorDetailsRequested(SensorItem sensor);
    }

    /**
     * ViewHolder class that represents each sensor item within a RecyclerView.
     * This class holds references to the views for each data item, which allows
     * modifications without creating a new view.
     */
    public static class SensorViewHolder extends RecyclerView.ViewHolder {
        ImageView sensorIcon;
        TextView sensorName;
        SwitchMaterial sensorSwitch;
        ImageButton sensorDetailsButton;

        public SensorViewHolder(View v) {
            super(v);
            sensorIcon = v.findViewById(R.id.sensor_icon);
            sensorName = v.findViewById(R.id.sensor_name);
            sensorSwitch = v.findViewById(R.id.sensor_switch);
            sensorDetailsButton = v.findViewById(R.id.sensor_details_button);
        }
    }

}
