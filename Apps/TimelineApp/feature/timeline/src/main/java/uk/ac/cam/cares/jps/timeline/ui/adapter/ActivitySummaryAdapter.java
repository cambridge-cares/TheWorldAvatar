package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.SummaryActivityItem;
import uk.ac.cam.cares.jps.timelinemap.R;
import android.widget.ImageView;
import android.widget.TextView;

public class ActivitySummaryAdapter extends RecyclerView.Adapter<ActivitySummaryAdapter.ActivitySummaryViewHolder> {
    private List<SummaryActivityItem> summaryList;

    public ActivitySummaryAdapter() {
        this.summaryList = new ArrayList<>();
    }
    
    public void setActivityItemList(List<SummaryActivityItem> summaryList) {
        this.summaryList = new ArrayList<>(summaryList);
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
        SummaryActivityItem summaryItem = summaryList.get(position);

        holder.activityType.setImageResource(summaryItem.getActivityImage());
        holder.activityType.setVisibility(View.VISIBLE);
        holder.totalDistance.setText(summaryItem.getTotalDistance());
        holder.timeSummary.setText(summaryItem.getTotalTimeSummary());
        
        
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