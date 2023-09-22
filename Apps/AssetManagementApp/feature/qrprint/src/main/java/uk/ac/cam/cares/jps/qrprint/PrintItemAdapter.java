package uk.ac.cam.cares.jps.qrprint;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.data.qrprint.PrintItem;

public class PrintItemAdapter extends RecyclerView.Adapter<PrintItemAdapter.ViewHolder>{
    List<PrintItem> items;

    PrintItemAdapter(List<PrintItem> items) {
        this.items = items;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.view_item, parent, false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        PrintItem currentItem = items.get(position);
        holder.labelTv.setText(currentItem.getLabel());
        holder.idTv.setText(currentItem.getInventoryID());
    }

    @Override
    public int getItemCount() {
        return items.size();
    }

    public void updateItems(List<PrintItem> newItems) {
        items = newItems;
        notifyDataSetChanged();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView idTv;
        TextView labelTv;
        ImageButton removeButton;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);

            idTv = itemView.findViewById(R.id.id_tv);
            labelTv = itemView.findViewById(R.id.label_tv);
            removeButton = itemView.findViewById(R.id.remove_bt);
        }
    }
}
