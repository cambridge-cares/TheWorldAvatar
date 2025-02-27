package uk.ac.cam.cares.jps.timeline.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.model.UniqueSessions;
import uk.ac.cam.cares.jps.timelinemap.R;

public class UniqueSessionsAdapter extends RecyclerView.Adapter<UniqueSessionsAdapter.UniqueSessionsViewHolder> {
    private List<UniqueSessions> uniqueSessionsList;
    private final RecyclerView.RecycledViewPool activitySummaryViewPool = new RecyclerView.RecycledViewPool();

    public UniqueSessionsAdapter() {
        this.uniqueSessionsList = new ArrayList<>();
    }

    public void setUniqueSessionsList(List<UniqueSessions> uniqueSessionsList) {
        this.uniqueSessionsList = new ArrayList<>(uniqueSessionsList);
        notifyDataSetChanged(); 
    }

    @NonNull
    @Override
    public UniqueSessionsViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.unique_session, parent, false);
        return new UniqueSessionsViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull UniqueSessionsViewHolder holder, int position) {
        UniqueSessions session = uniqueSessionsList.get(position);
        holder.sessionTitle.setText(session.getSessionTitle());

        LinearLayoutManager layoutManager = new LinearLayoutManager(
                holder.activitySummaryRecyclerView.getContext(),
                LinearLayoutManager.VERTICAL,
                false
        );
        layoutManager.setInitialPrefetchItemCount(session.getShownList().size());

        ActivityItemAdapter activityItemAdapter = new ActivityItemAdapter(session.getShownList());
        holder.activitySummaryRecyclerView.setLayoutManager(layoutManager);
        holder.activitySummaryRecyclerView.setAdapter(activityItemAdapter);
        holder.activitySummaryRecyclerView.setRecycledViewPool(activitySummaryViewPool);

        holder.dropdownLayout.setOnClickListener(view -> {
            if (!session.getShownList().isEmpty()) {
                session.setShownListAsEmptyList();
            } else {
                session.setShownListAsActivityList();
            }

            notifyItemChanged(position);
        });
    }

    @Override
    public int getItemCount() {
        return uniqueSessionsList.size();
    }

    public static class UniqueSessionsViewHolder extends RecyclerView.ViewHolder {
        TextView sessionTitle;
        RecyclerView activitySummaryRecyclerView;
        View dropdownLayout;

        public UniqueSessionsViewHolder(@NonNull View sessionView) {
            super(sessionView);
            sessionTitle = sessionView.findViewById(R.id.session_id);
            activitySummaryRecyclerView = sessionView.findViewById(R.id.activity_items);
            dropdownLayout = sessionView.findViewById(R.id.session_dropdown_layout); 
        }
    }
}

