package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.model.ActivityItem;
import uk.ac.cam.cares.jps.timelinemap.R;
/**
 * Adapter class for recyclerview to view activity summary information.
 */
public class ActivitySummaryAdapter extends RecyclerView.Adapter<ActivitySummaryAdapter.ActivitySummaryViewHolder> {
    private List<ActivityItem> activityList;

    public ActivitySummaryAdapter() {
        activityList = new ArrayList<>();
    }

    public void setActivityList(List<ActivityItem> activities) {
        activityList = activities;
    }

    @NonNull
    @Override
    public ActivitySummaryViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.activity_item, parent, false);
        return new ActivitySummaryViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ActivitySummaryViewHolder holder, int position) {
        ActivityItem activityItem = activityList.get(position);

        holder.activityType.setImageResource(activityItem.getActivityImage());
        holder.activityType.setVisibility(View.VISIBLE);
        holder.timeSummary.setText(activityItem.getTimeSummary());
        
        
    }

    @Override
    public int getItemCount() {
        return activityList.size();
    }

    static class ActivitySummaryViewHolder extends RecyclerView.ViewHolder {
        ImageView activityType;
        TextView timeSummary;

        public ActivitySummaryViewHolder(@NonNull View activityView) {
            super(activityView);

            activityType = activityView.findViewById(R.id.activityType);
            timeSummary = activityView.findViewById(R.id.timeSummary);
        }
    }

}