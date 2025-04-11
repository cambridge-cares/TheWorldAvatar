package uk.ac.cam.cares.jps.timeline.ui.adapter;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.content.Context;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timelinemap.R;


public class SessionsAdapter extends RecyclerView.Adapter<SessionsAdapter.SessionsViewHolder> {
    private List<Session> sessionList;
    private TrajectorySegment clickedSegment;
    private final RecyclerView.RecycledViewPool sharedViewPool = new RecyclerView.RecycledViewPool();

    public SessionsAdapter() {
        this.sessionList = new ArrayList<>();
    }

    public void setUniqueSessionsList(List<Session> sessionList, TrajectorySegment clickedSegment) {
        this.sessionList = new ArrayList<>(sessionList);
        this.clickedSegment = clickedSegment;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public SessionsViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.unique_session, parent, false);
        return new SessionsViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SessionsViewHolder holder, int position) {
        Session session = sessionList.get(position);
        holder.sessionTitle.setText(session.getSessionTitle());

        NonScrollableLinearLayoutManager layoutManager = new NonScrollableLinearLayoutManager(holder.activityRecyclerView.getContext());

        ActivityItemAdapter activityItemAdapter = new ActivityItemAdapter(session.getActivityList(), session.getSessionId(), clickedSegment);
        holder.activityRecyclerView.setLayoutManager(layoutManager);
        holder.activityRecyclerView.setAdapter(activityItemAdapter);
        holder.activityRecyclerView.setRecycledViewPool(sharedViewPool);
        holder.activityRecyclerView.setHasFixedSize(false);

        holder.activityRecyclerView.post(() -> {
            ViewGroup.LayoutParams params = holder.activityRecyclerView.getLayoutParams();

            if (clickedSegment != null) {
                int maxHeightPx = (int) TypedValue.applyDimension(
                        TypedValue.COMPLEX_UNIT_DIP, 500,
                        holder.activityRecyclerView.getResources().getDisplayMetrics()
                );

                // Measure content height
                int totalHeight = 0;
                for (int i = 0; i < holder.activityRecyclerView.getChildCount(); i++) {
                    View child = holder.activityRecyclerView.getChildAt(i);
                    if (child != null) {
                        totalHeight += child.getMeasuredHeight();
                    }
                }

                params.height = Math.min(totalHeight, maxHeightPx); // Dynamic sizing
            } else {
                params.height = ViewGroup.LayoutParams.MATCH_PARENT; // Full height if no clickedSegment
            }

            holder.activityRecyclerView.setLayoutParams(params);
        });

        // Ensure we scroll to the correct activity within the session
        if (clickedSegment != null && session.containsSegment(clickedSegment)) {
            holder.activityRecyclerView.post(() ->
                    layoutManager.scrollToPositionWithOffset(clickedSegment.getNumberInSession() - 1, 0)
            );
        }
    }

    @Override
    public int getItemCount() {
        return sessionList.size();
    }

    public void setClickedSegment(TrajectorySegment clickedSegment) {
        this.clickedSegment = clickedSegment;

        for (int i = 0; i < sessionList.size(); i++) {
            Session session = sessionList.get(i);
            if (session.containsSegment(clickedSegment) || clickedSegment == null) {
                notifyItemChanged(i);
                return;
            }
        }
    }

    public class NonScrollableLinearLayoutManager extends LinearLayoutManager {

        public NonScrollableLinearLayoutManager(Context context) {
            super(context, VERTICAL, false);
        }

        @Override
        public boolean canScrollVertically() {
            return false;
        }
    }

    public static class SessionsViewHolder extends RecyclerView.ViewHolder {
        TextView sessionTitle;
        RecyclerView activityRecyclerView;
        View dropdownLayout;

        public SessionsViewHolder(@NonNull View sessionView) {
            super(sessionView);
            sessionTitle = sessionView.findViewById(R.id.session_id);
            activityRecyclerView = sessionView.findViewById(R.id.activity_items);
            dropdownLayout = sessionView.findViewById(R.id.session_dropdown_layout);

            dropdownLayout.setOnClickListener(view -> {
                if (activityRecyclerView.getVisibility() == VISIBLE) {
                    activityRecyclerView.setVisibility(GONE);
                } else {
                    activityRecyclerView.setVisibility(VISIBLE);
                }
            });
        }
    }
}

