package uk.ac.cam.cares.jps.user.viewmodel;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Switch;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.user.OnSensorToggleListener;
import uk.ac.cam.cares.jps.user.R;
import uk.ac.cam.cares.jps.user.SensorItem;
import uk.ac.cam.cares.jps.user.SensorSettingFragment;

/**
 * Adapter class for managing and displaying a list of sensor items in a RecyclerView.
 * This adapter binds the sensor data to the views and handles user interactions with the sensor toggle switches.
 */
public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {
    private final OnSensorToggleListener listener;
    private List<SensorItem> sensorItems;

    /**
     * Constructor for the SensorAdapter.
     *
     * @param sensorItems The list of sensor items to be displayed.
     * @param listener    The listener to handle sensor toggle events.
     */
    public SensorAdapter(List<SensorItem> sensorItems,  OnSensorToggleListener listener) {
        this.sensorItems = sensorItems;
        this.listener = listener;
    }

    /**
     * Creates a new ViewHolder object for a sensor item.
     *
     * @param parent   The parent ViewGroup into which the new view will be added after it is bound to an adapter position.
     * @param viewType The view type of the new view.
     * @return A new SensorViewHolder that holds a view of the sensor item.
     */
    @NonNull
    @Override
    public SensorAdapter.SensorViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.sensor_item, parent, false);
        return new SensorViewHolder(view);
    }

    /**
     * Binds the sensor data to the views in the ViewHolder.
     *
     * @param holder   The ViewHolder that should be updated to represent the contents of the item at the given position in the data set.
     * @param position The position of the item within the adapter's data set.
     */

    @Override
    public void onBindViewHolder(@NonNull SensorViewHolder holder, int position) {
        SensorItem sensorItem = sensorItems.get(position);
        holder.sensorText.setText(sensorItem.getSensorName());
        holder.sensorToggleSwitch.setChecked(sensorItem.isToggled());
        holder.sensorDescription.setText(sensorItem.getSensorDescription());
        holder.sensorToggleSwitch.setEnabled(sensorItem.isToggleEnabled()); // Disable the toggle if needed

        holder.sensorToggleSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            if (sensorItem.isToggleEnabled()) { // Only allow toggling if enabled
                sensorItem.setToggled(isChecked);
                listener.onSensorToggle();
            }
        });
    }

    /**
     * Returns the total number of sensor items in the adapter.
     *
     * @return The total number of sensor items in the data set held by the adapter.
     */
    @Override
    public int getItemCount() {
        return sensorItems.size();
    }

    /**
     * Returns the list of sensor items managed by the adapter.
     *
     * @return The list of sensor items.
     */
    public List<SensorItem> getSensorItems() {
        return sensorItems;
    }


    /**
     * ViewHolder class that represents each sensor item in the RecyclerView.
     */
    static class SensorViewHolder extends RecyclerView.ViewHolder {
        TextView sensorText;
        Switch sensorToggleSwitch;
        TextView sensorDescription;

        public SensorViewHolder(@NonNull View itemView){
            super(itemView);
            sensorText = itemView.findViewById(R.id.sensor_name);
            sensorToggleSwitch = itemView.findViewById(R.id.sensor_toggle);
            sensorDescription = itemView.findViewById(R.id.sensor_description);
        }
    }

    /**
     * Enables or disables the toggle switches for all sensor items in the adapter.
     *
     * @param enabled If true, all toggle switches will be enabled; if false, they will be disabled.
     */
    public void setTogglesEnabled(boolean enabled) {
        for (SensorItem sensorItem : sensorItems) {
            sensorItem.setToggleEnabled(enabled);
        }
        notifyDataSetChanged();
    }

}
