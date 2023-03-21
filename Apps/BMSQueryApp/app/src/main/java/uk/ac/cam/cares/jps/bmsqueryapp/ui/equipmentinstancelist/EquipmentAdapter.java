package uk.ac.cam.cares.jps.bmsqueryapp.ui.equipmentinstancelist;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.R;

public class EquipmentAdapter extends RecyclerView.Adapter<EquipmentAdapter.EquipmentViewHolder> {

    public static class EquipmentViewHolder extends RecyclerView.ViewHolder {
        private final TextView textView;
        private final View view;

        public EquipmentViewHolder(View view) {
            super(view);
            // Define click listener for the ViewHolder's View

            textView = view.findViewById(R.id.label_tv);
            this.view = view;
        }

        public TextView getTextView() {
            return textView;
        }

        public View getView() {
            return view;
        }
    }

    private List<String> equipmentLabels;
    private OnEquipmentClickedListener listener;

    public EquipmentAdapter(List<String> equipmentLabels, OnEquipmentClickedListener listener) {
        this.equipmentLabels = equipmentLabels;
        this.listener = listener;
    }

    @NonNull
    @Override
    public EquipmentViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.equip_instance_row_view, parent, false);

        EquipmentViewHolder viewHolder = new EquipmentViewHolder(view);

        return viewHolder;
    }

    @Override
    public void onBindViewHolder(@NonNull EquipmentViewHolder holder, int position) {
        holder.getTextView().setText(equipmentLabels.get(position));
        holder.getView().setOnClickListener(view -> {
            listener.onItemClicked(holder);
        });
    }

    @Override
    public int getItemCount() {
        return equipmentLabels.size();
    }

    public void updateEquipments(List<String> newEquipmentLabels) {
        this.equipmentLabels = newEquipmentLabels;
        this.notifyDataSetChanged();
    }
}
