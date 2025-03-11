package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivityItem;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timelinemap.R;
/**
 * Adapter class for recyclerview to view activity summary information.
 */
public class ActivityItemAdapter extends RecyclerView.Adapter<ActivityItemAdapter.ActivityItemViewHolder> {
    private final List<ActivityItem> activityList;

    public ActivityItemAdapter(List<ActivityItem> activityList, Integer clickedId) {
        this.activityList = activityList;

        for(ActivityItem item : activityList) {
            if(item.getId() == clickedId) {
                item.setClicked(!item.getClicked());
            }
            else {
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
        holder.activityType.setVisibility(View.VISIBLE);
        holder.timeSummary.setText(activityItem.getTimeSummary());

        holder.clicked.setImageResource(R.drawable.baseline_keyboard_double_arrow_left_24);
        if(activityItem.getClicked()) {
            holder.clicked.setVisibility(View.VISIBLE);
        }
        else {
            holder.clicked.setVisibility(View.GONE);
        }
        
    }

    @Override
    public int getItemCount() {
        return activityList.size();
    }

    static class ActivityItemViewHolder extends RecyclerView.ViewHolder {
        ImageView activityType;
        TextView timeSummary;
        ImageView clicked;

        public ActivityItemViewHolder(@NonNull View activityView) {
            super(activityView);

            activityType = activityView.findViewById(R.id.activityType);
            timeSummary = activityView.findViewById(R.id.timeSummary);
            clicked = activityView.findViewById(R.id.clicked);
        }
    }

}