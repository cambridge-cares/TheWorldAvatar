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

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timelinemap.R;

// public class SessionsAdapter extends RecyclerView.Adapter<SessionsAdapter.SessionsViewHolder> {
//     private List<Session> sessionList;
//     private Integer clickedId;
//     private final RecyclerView.RecycledViewPool activitySummaryViewPool = new RecyclerView.RecycledViewPool();

//     public SessionsAdapter() {
//         this.sessionList = new ArrayList<>();
//     }

//     public void setUniqueSessionsList(List<Session> sessionList, Integer clickedId) {
//         this.sessionList = new ArrayList<>(sessionList);
//         this.clickedId = clickedId;
//         notifyDataSetChanged(); 
//     }

//     @NonNull
//     @Override
//     public SessionsViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
//         View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.unique_session, parent, false);
//         return new SessionsViewHolder(view);
//     }

//     @Override
//     public void onBindViewHolder(@NonNull SessionsViewHolder holder, int position) {
//         Session session = sessionList.get(position);
//         holder.sessionTitle.setText(session.getSessionTitle());

//         LinearLayoutManager layoutManager = new LinearLayoutManager(
//                 holder.activitySummaryRecyclerView.getContext(),
//                 LinearLayoutManager.VERTICAL,
//                 false
//         );
//         layoutManager.setInitialPrefetchItemCount(session.getShownList().size());

//         ActivityItemAdapter activityItemAdapter = new ActivityItemAdapter(session.getShownList(), clickedId);
//         holder.activitySummaryRecyclerView.setLayoutManager(layoutManager);
//         holder.activitySummaryRecyclerView.setAdapter(activityItemAdapter);
//         holder.activitySummaryRecyclerView.setRecycledViewPool(activitySummaryViewPool);

//         holder.dropdownLayout.setOnClickListener(view -> {
//             if (!session.getShownList().isEmpty()) {
//                 session.setShownListAsEmptyList();
//             } else {
//                 session.setShownListAsActivityList();
//             }

//             notifyItemChanged(position);
//         });
//     }

//     @Override
//     public int getItemCount() {
//         return sessionList.size();
//     }

//     public void setClickedSegment(Integer clickedId) {
//         this.clickedId = clickedId;
//         notifyDataSetChanged();
//     }

//     public static class SessionsViewHolder extends RecyclerView.ViewHolder {
//         TextView sessionTitle;
//         RecyclerView activitySummaryRecyclerView;
//         View dropdownLayout;

//         public SessionsViewHolder(@NonNull View sessionView) {
//             super(sessionView);
//             sessionTitle = sessionView.findViewById(R.id.session_id);
//             activitySummaryRecyclerView = sessionView.findViewById(R.id.activity_items);
//             dropdownLayout = sessionView.findViewById(R.id.session_dropdown_layout); 
//         }
//     }
// }
public class SessionsAdapter extends RecyclerView.Adapter<SessionsAdapter.SessionsViewHolder> {
    private List<Session> sessionList;
    private TrajectorySegment clickedSegment;
    private final RecyclerView.RecycledViewPool activitySummaryViewPool = new RecyclerView.RecycledViewPool();

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

        LinearLayoutManager layoutManager = new LinearLayoutManager(
                holder.activitySummaryRecyclerView.getContext(),
                LinearLayoutManager.VERTICAL,
                false
        );
        layoutManager.setInitialPrefetchItemCount(session.getShownList().size());

        // Ensure ActivityItemAdapter receives the updated clickedId
        ActivityItemAdapter activityItemAdapter = new ActivityItemAdapter(session.getShownList(), clickedSegment);
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
        return sessionList.size();
    }

    public void setClickedSegment(TrajectorySegment clickedSegment) {
        this.clickedSegment = clickedSegment;
        // Find which session contains the clicked segment and update only that session
        for (int i = 0; i < sessionList.size(); i++) {
            Session session = sessionList.get(i);
            if (session.containsSegment(clickedSegment)) { // Assuming a helper method
                notifyItemChanged(i);
                return;
            }
        }
    }

    public static class SessionsViewHolder extends RecyclerView.ViewHolder {
        TextView sessionTitle;
        RecyclerView activitySummaryRecyclerView;
        View dropdownLayout;

        public SessionsViewHolder(@NonNull View sessionView) {
            super(sessionView);
            sessionTitle = sessionView.findViewById(R.id.session_id);
            activitySummaryRecyclerView = sessionView.findViewById(R.id.activity_items);
            dropdownLayout = sessionView.findViewById(R.id.session_dropdown_layout); 
        }
    }
}

