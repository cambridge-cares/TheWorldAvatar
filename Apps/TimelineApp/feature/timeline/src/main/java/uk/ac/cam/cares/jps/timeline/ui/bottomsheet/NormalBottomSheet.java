

package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.SummaryActivityItem;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.timeline.ui.adapter.ActivitySummaryAdapter;
import uk.ac.cam.cares.jps.timeline.ui.adapter.SessionsAdapter;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * Bottom sheet to be shown when there is no error
 */
public class NormalBottomSheet extends BottomSheet {

    private RecyclerView summaryRecyclerView;
    private ActivitySummaryAdapter summaryAdapter;
    private RecyclerView sessionsRecyclerView;
    private SessionsAdapter sessionsAdapter;

    public NormalBottomSheet(Context context) {
        super(context);
        init(context);
    }

    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget, null);

        sessionsRecyclerView = bottomSheet.findViewById(R.id.sessions_recycler_view);

        sessionsAdapter = new SessionsAdapter();
        sessionsRecyclerView.setAdapter(sessionsAdapter);
        sessionsRecyclerView.setLayoutManager(new LinearLayoutManager(context));

        summaryRecyclerView = bottomSheet.findViewById(R.id.summary_recycler_view);

        summaryAdapter = new ActivitySummaryAdapter();
        summaryRecyclerView.setHasFixedSize(true);
        summaryRecyclerView.setNestedScrollingEnabled(true);
        summaryRecyclerView.setVerticalScrollBarEnabled(false);
        summaryRecyclerView.setHorizontalScrollBarEnabled(true);
        summaryRecyclerView.setScrollbarFadingEnabled(false);
        summaryRecyclerView.setAdapter(summaryAdapter);
        summaryRecyclerView.setLayoutManager(new LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false));
    }

    public void showFetchingAnimation(boolean isFetching) {
        if (isFetching) {
            sessionsAdapter = new SessionsAdapter();
            sessionsRecyclerView.setAdapter(sessionsAdapter);

            summaryAdapter = new ActivitySummaryAdapter();
            summaryRecyclerView.setAdapter(summaryAdapter);

            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.VISIBLE);
            getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.GONE);
            sessionsRecyclerView.setVisibility(View.GONE);
            summaryRecyclerView.setVisibility(View.GONE);


        } else {
            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.GONE);
            summaryRecyclerView.setVisibility(View.VISIBLE);
            sessionsRecyclerView.setVisibility(View.VISIBLE);
        }
    }

    public void updateUniqueSessionsList(List<Session> sessionList, TrajectorySegment clickedId) {
        if (sessionList != null && !sessionList.isEmpty()) {
            sessionsAdapter.setUniqueSessionsList(sessionList, clickedId);
            sessionsAdapter.notifyDataSetChanged();

            TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
            if (trajectoryTextView != null) {
                trajectoryTextView.setVisibility(View.GONE);
            }
        } else {
            showEmptyState();
        }
    }

    public void updateSummaryView(List<SummaryActivityItem> summaryActivityItemList) {
        if (summaryActivityItemList != null && !summaryActivityItemList.isEmpty()) {
            summaryAdapter.setActivityItemList(summaryActivityItemList);
            summaryAdapter.notifyDataSetChanged();

            TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
            if (trajectoryTextView != null) {
                trajectoryTextView.setVisibility(View.GONE);
            }
        }
    }

    private void showEmptyState() {
        sessionsRecyclerView.setVisibility(View.GONE);
        summaryRecyclerView.setVisibility(View.GONE);

        TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
        if (trajectoryTextView != null) {
            trajectoryTextView.setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
            trajectoryTextView.setVisibility(View.VISIBLE);
        }
    }

    public void highlightClickedSegment(TrajectorySegment clickedSegment) {
        sessionsAdapter.setClickedSegment(clickedSegment);
        sessionsAdapter.notifyDataSetChanged();
    }
}
