package uk.ac.cam.cares.jps.bmsqueryapp.adapter.list;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.R;
import uk.ac.cam.cares.jps.bmsqueryapp.data.buildings.Equipment;

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

    private List<Equipment> equipmentList;
    private OnEquipmentClickedListener listener;

    public EquipmentAdapter(List<Equipment> equipmentList, OnEquipmentClickedListener listener) {
        this.equipmentList = equipmentList;
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
        holder.getTextView().setText(equipmentList.get(position).getLabel());
        holder.getView().setOnClickListener(view -> {
            listener.onItemClicked(holder, equipmentList.get(position));
        });
    }

    @Override
    public int getItemCount() {
        return equipmentList.size();
    }

    public void updateEquipments(List<Equipment> newEquipmentLabels) {
        this.equipmentList = newEquipmentLabels;
        this.notifyDataSetChanged();
    }

    public void clear() {
        this.equipmentList.clear();
        this.notifyDataSetChanged();
    }
}
