package uk.ac.cam.cares.jps.qrprint;

import android.view.View;

import androidx.annotation.NonNull;

import java.util.List;

import uk.ac.cam.cares.jps.data.qrprint.PrintItem;

public class RemovablePrintItemAdapter extends PrintItemAdapter{
    private QRPrintingFragment.ListUpdate listUpdate;
    private int drawableId;
    private int currentVisibility = View.GONE;

    RemovablePrintItemAdapter(List<PrintItem> items, QRPrintingFragment.ListUpdate listUpdate, int drawableId) {
        super(items);
        this.listUpdate = listUpdate;
        this.drawableId = drawableId;
    }

    public void setCurrentVisibility(int visibility) {
        currentVisibility = visibility;
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        super.onBindViewHolder(holder, position);
        holder.removeButton.setOnClickListener(v -> listUpdate.removeItemFromPrintList(items.get(position)));
        holder.removeButton.setBackgroundResource(drawableId);
        holder.removeButton.setVisibility(currentVisibility);
    }
}
