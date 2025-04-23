package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.graphics.Color;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivitySummary;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timelinemap.R;

import android.widget.ImageView;
import android.widget.TextView;

public class ActivitySummaryAdapter extends RecyclerView.Adapter<ActivitySummaryAdapter.ActivitySummaryViewHolder> {
    private List<ActivitySummary> summaryList;

    public ActivitySummaryAdapter() {
        this.summaryList = new ArrayList<>();
    }

    public void setActivityItemList(List<ActivitySummary> summaryList) {
        this.summaryList = new ArrayList<>(summaryList);

    }

    @NonNull
    @Override
    public ActivitySummaryViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.summary_info, parent, false);
        return new ActivitySummaryViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ActivitySummaryViewHolder holder, int position) {
        ActivitySummary summaryItem = summaryList.get(position);

        holder.activityType.setImageResource(summaryItem.getActivityImage());

        List<String> validActivities = new ArrayList<>();
        validActivities.add("still");
        validActivities.add("walking");
        validActivities.add("bike");
        validActivities.add("vehicle");


        holder.totalDistance.setText(summaryItem.getTotalDistance());
        holder.timeSummary.setText(summaryItem.getTimeSummary());

        if (validActivities.contains(summaryItem.getActivityType())) {
            holder.activityType.setVisibility(View.VISIBLE);
        } else {
            holder.activityType.setVisibility(View.GONE);
            holder.totalDistance.setVisibility(View.GONE);
            holder.timeSummary.setVisibility(View.GONE);
        }

    }

    @Override
    public int getItemCount() {
        return summaryList.size();
    }

    static class ActivitySummaryViewHolder extends RecyclerView.ViewHolder {
        ImageView activityType;
        TextView totalDistance;
        TextView timeSummary;

        public ActivitySummaryViewHolder(@NonNull View summaryView) {
            super(summaryView);

            activityType = summaryView.findViewById(R.id.activity_type);
            totalDistance = summaryView.findViewById(R.id.total_distance);
            timeSummary = summaryView.findViewById(R.id.total_time);
        }
    }
}