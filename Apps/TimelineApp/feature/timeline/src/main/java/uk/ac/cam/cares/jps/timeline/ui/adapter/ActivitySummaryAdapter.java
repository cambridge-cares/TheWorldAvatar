package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.graphics.Color;
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
    private TrajectorySegment clickedSegment;

    public ActivitySummaryAdapter() {
        this.summaryList = new ArrayList<>();
    }

    public void setActivityItemList(List<ActivitySummary> summaryList, TrajectorySegment clickedSegment) {
        this.summaryList = new ArrayList<>(summaryList);
        this.clickedSegment = clickedSegment;

         for(ActivitySummary activity : summaryList) {
            if(clickedSegment == null) {
                activity.setClicked(false);
            }
            else if(activity.getActivityType().equals(clickedSegment.getActivityType())) {
                activity.setClicked(true);
            }
            else {
                activity.setClicked(false);
            }
        }
        notifyDataSetChanged();
    }

    private void setClickedSegment(TrajectorySegment clickedSegment) {
        this.clickedSegment = clickedSegment;

         for(ActivitySummary activity : summaryList) {
            if(clickedSegment == null) {
                activity.setClicked(false);
            }
            else if(activity.getActivityType().equals(clickedSegment.getActivityType())) {
                activity.setClicked(true);
            }
            else {
                activity.setClicked(false);
            }
        }
        notifyDataSetChanged();
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

        if(validActivities.contains(summaryItem.getActivityType())) {
            holder.activityType.setVisibility(View.VISIBLE);
        }
        else {
            holder.activityType.setVisibility(View.GONE);
            holder.totalDistance.setVisibility(View.GONE);
            holder.timeSummary.setVisibility(View.GONE);
        }

        if (summaryItem.isClicked()) {
            holder.activityType.setBackgroundColor(Color.YELLOW);
        } else {
            holder.activityType.setBackgroundColor(Color.TRANSPARENT);
        }
    
    }

    @Override
    public int getItemCount() {
        return summaryList.size();
    }

    public void highlightClickedActivity(TrajectorySegment clickedSegment) {
        setClickedSegment(clickedSegment);

        for (int i = 0; i < summaryList.size(); i++) {
            ActivitySummary summary = summaryList.get(i);
            if (clickedSegment == null || summary.getActivityType().equals(clickedSegment.getActivityType())) {
                notifyItemChanged(i);
                return;
            }
        }
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