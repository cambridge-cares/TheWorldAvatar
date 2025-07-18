package uk.ac.cam.cares.jps.user.viewmodel;

import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Switch;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.user.R;
import uk.ac.cam.cares.jps.user.SensorItem;

/**
 * Adapter class for managing and displaying a list of sensor items in a RecyclerView.
 * This adapter binds the sensor data to the views and handles user interactions with the sensor toggle switches.
 */
public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {
    private final SensorViewModel sensorViewModel;
    private List<SensorItem> sensorItems;

    /**
     * Constructor for the SensorAdapter.
     *
     * @param sensorItems   The list of sensor items to be displayed.
     */
    public SensorAdapter(List<SensorItem> sensorItems, SensorViewModel sensorViewModel) {
        this.sensorItems = sensorItems;
        this.sensorViewModel = sensorViewModel;
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
        holder.sensorDescription.setText(sensorItem.getSensorDescription());

        // Always keep switches visually active so the user can see toggle state clearly
        holder.sensorToggleSwitch.setEnabled(true);  // avoid grey-out effect

        // Temporarily remove the listener to prevent misfire
        holder.sensorToggleSwitch.setOnCheckedChangeListener(null);
        holder.sensorToggleSwitch.setChecked(sensorItem.isToggled());

        // Reattach the listener only if toggle is enabled
        if (sensorItem.isToggleEnabled()) {
            holder.sensorToggleSwitch.setOnTouchListener(null);  // allow interaction
            holder.sensorToggleSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                sensorItem.setToggled(isChecked);  // Update the sensor item toggle state
                sensorViewModel.toggleSensor(sensorItem.getSensorType());  // Update ViewModel with the toggle state
            });
        } else {
            // Block interaction without greying out the switch by intercepting all touch events
            holder.sensorToggleSwitch.setOnTouchListener((v, event) -> true);  // consume touch, prevent toggle
        }
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

    public void updateSensorItems(List<SensorItem> newSensorItems) {
        this.sensorItems.clear();
        this.sensorItems.addAll(newSensorItems);  // Replace the old items with the new ones
    }
}
