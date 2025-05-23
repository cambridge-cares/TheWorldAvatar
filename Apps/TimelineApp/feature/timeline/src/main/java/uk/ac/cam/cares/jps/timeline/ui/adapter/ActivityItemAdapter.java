package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;
import java.util.ArrayList;


import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivityItem;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timeline.viewmodel.SegmentClickInterface;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * Adapter class for recyclerview to view activity summary information.
 */
public class ActivityItemAdapter extends RecyclerView.Adapter<ActivityItemAdapter.ActivityItemViewHolder> {
    private final List<ActivityItem> activityList;
    private final SegmentClickInterface segmentClickInterface;
    private String sessionId;

    public ActivityItemAdapter(List<ActivityItem> activityList,
                               String sessionId,
                               TrajectorySegment clickedSegment,
                               SegmentClickInterface segmentClickInterface) {
        this.activityList = activityList;
        this.segmentClickInterface = segmentClickInterface;
        this.sessionId = sessionId;

        for (ActivityItem item : activityList) {
            if (clickedSegment == null) {
                item.setClicked(false);
            } else if (item.getId() == clickedSegment.getId() && sessionId.equals(clickedSegment.getSessionId())) {
                item.setClicked(true);
            } else {
                item.setClicked(false);
            }
        }
    }

    @NonNull
    @Override
    public ActivityItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.activity_item, parent, false);
        return new ActivityItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ActivityItemViewHolder holder, int position) {
        ActivityItem activityItem = activityList.get(position);

        holder.activityType.setImageResource(activityItem.getActivityImage());

        List<String> validActivities = new ArrayList<>();
        validActivities.add("still");
        validActivities.add("walking");
        validActivities.add("bike");
        validActivities.add("vehicle");
        validActivities.add("unknown");


        if (validActivities.contains(activityItem.getActivityType())) {
            holder.activityType.setVisibility(View.VISIBLE);
        } else {
            holder.activityType.setVisibility(View.GONE);
        }

        holder.timeSummary.setText(activityItem.getTimeSummary());

        if (activityItem.getClicked()) {
            holder.clicked.setVisibility(View.VISIBLE);
        } else {
            holder.clicked.setVisibility(View.GONE);
        }

        holder.view.setOnClickListener(v -> segmentClickInterface.setClickedSegment(activityItem.getId(), sessionId));

    }

    @Override
    public int getItemCount() {
        return activityList.size();
    }

    public int getFirstClickedItemPosition() {
        for (int i = 0; i < activityList.size(); i++) {
            if (activityList.get(i).getClicked()) {
                return i;
            }
        }
        return RecyclerView.NO_POSITION;
    }

    static class ActivityItemViewHolder extends RecyclerView.ViewHolder {
        ImageView activityType;
        TextView timeSummary;
        ImageView clicked;
        View view;

        public ActivityItemViewHolder(@NonNull View activityView) {
            super(activityView);

            activityType = activityView.findViewById(R.id.activityType);
            timeSummary = activityView.findViewById(R.id.timeSummary);
            clicked = activityView.findViewById(R.id.clicked);

            view = activityView;
        }
    }

}