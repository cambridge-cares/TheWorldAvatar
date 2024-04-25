package com.example.notsensorlogger2;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.switchmaterial.SwitchMaterial;

import java.util.List;

public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {

    private List<SensorItem> sensorsList; // Populate this list with your sensor data
    private OnSensorToggleListener toggleListener;
    private OnSensorDetailsListener detailsListener;

    public interface OnSensorToggleListener {
        void onSensorToggled(SensorItem sensor, boolean isChecked);
    }

    public interface OnSensorDetailsListener {
        void onSensorDetailsRequested(SensorItem sensor);
    }

    // Provide a reference to the views for each data item
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

    // Constructor
    public SensorAdapter(List<SensorItem> myDataset, OnSensorToggleListener toggleListener, OnSensorDetailsListener detailsListener) {
        sensorsList = myDataset;
        this.toggleListener = toggleListener;
        this.detailsListener = detailsListener;
    }

    // Create new views (invoked by the layout manager)
    @Override
    public SensorViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_sensor, parent, false);
        return new SensorViewHolder(v);
    }

    // Replace the contents of a view (invoked by the layout manager)
    @Override
    public void onBindViewHolder(SensorViewHolder holder, int position) {
        SensorItem sensorItem = sensorsList.get(position);
        holder.sensorName.setText(sensorItem.getSensorName());
        holder.sensorIcon.setImageResource(sensorItem.getSensorIconResId());
        holder.sensorSwitch.setChecked(sensorItem.isToggled());

        // Detach the listener before setting checked state to prevent unwanted callbacks
        holder.sensorSwitch.setOnCheckedChangeListener(null);
        holder.sensorSwitch.setChecked(sensorItem.isToggled());
        holder.sensorSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            // Notify any external listener
            if (toggleListener != null) {
                toggleListener.onSensorToggled(sensorItem, isChecked);
            }
            // Update the enabled state of the sensor
            sensorItem.setEnabled(isChecked);
        });

        holder.sensorDetailsButton.setOnClickListener(view -> {
            if (detailsListener != null) {
                detailsListener.onSensorDetailsRequested(sensorItem);
            }
        });
    }

    // Return the size of your dataset (invoked by the layout manager)
    @Override
    public int getItemCount() {
        return sensorsList.size();
    }
}
































//package com.example.notsensorlogger2;
//
//import android.view.LayoutInflater;
//import android.view.View;
//import android.view.ViewGroup;
//import android.widget.ImageButton;
//import android.widget.ImageView;
//import android.widget.TextView;
//
//import androidx.recyclerview.widget.RecyclerView;
//
//import com.google.android.material.switchmaterial.SwitchMaterial;
//
//import java.util.List;
//
//public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {
//
//    private List<SensorItem> sensorsList; // Populate this list with your sensor data
//    private OnSensorToggleListener toggleListener;
//    private OnSensorDetailsListener detailsListener;
//
//    public interface OnSensorToggleListener {
//        void onSensorToggled(SensorItem sensor, boolean isChecked);
//    }
//
//    public interface OnSensorDetailsListener {
//        void onSensorDetailsRequested(SensorItem sensor);
//    }
//
//    // Provide a reference to the views for each data item
//    public static class SensorViewHolder extends RecyclerView.ViewHolder {
//        ImageView sensorIcon;
//        TextView sensorName;
//        SwitchMaterial sensorSwitch;
//        ImageButton sensorDetailsButton;
//
//        public SensorViewHolder(View v) {
//            super(v);
//            sensorIcon = v.findViewById(R.id.sensor_icon);
//            sensorName = v.findViewById(R.id.sensor_name);
//            sensorSwitch = v.findViewById(R.id.sensor_switch);
//            sensorDetailsButton = v.findViewById(R.id.sensor_details_button);
//        }
//    }
//
//    // Constructor
//    public SensorAdapter(List<SensorItem> myDataset, OnSensorToggleListener toggleListener, OnSensorDetailsListener detailsListener) {
//        sensorsList = myDataset;
//        this.toggleListener = toggleListener;
//        this.detailsListener = detailsListener;
//    }
//
//    // Create new views (invoked by the layout manager)
//    @Override
//    public SensorViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
//        View v = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_sensor, parent, false);
//        return new SensorViewHolder(v);
//    }
//
//    // Replace the contents of a view (invoked by the layout manager)
//    @Override
//    public void onBindViewHolder(SensorViewHolder holder, int position) {
//        SensorItem sensorItem = sensorsList.get(position);
//        holder.sensorName.setText(sensorItem.getSensorName());
//        holder.sensorIcon.setImageResource(sensorItem.getSensorIconResId());
//        holder.sensorSwitch.setChecked(sensorItem.isToggled());
//        holder.sensorSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> toggleListener.onSensorToggled(sensorItem, isChecked));
//        holder.sensorDetailsButton.setOnClickListener(view -> detailsListener.onSensorDetailsRequested(sensorItem));
//    }
//
//    // Return the size of your dataset (invoked by the layout manager)
//    @Override
//    public int getItemCount() {
//        return sensorsList.size();
//    }
//}