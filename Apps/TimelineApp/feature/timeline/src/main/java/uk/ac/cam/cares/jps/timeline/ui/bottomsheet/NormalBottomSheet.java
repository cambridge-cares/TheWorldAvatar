

package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static com.google.android.material.bottomsheet.BottomSheetBehavior.STATE_COLLAPSED;
import static com.google.android.material.bottomsheet.BottomSheetBehavior.STATE_EXPANDED;
import static com.google.android.material.bottomsheet.BottomSheetBehavior.STATE_HALF_EXPANDED;

import android.content.Context;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.core.view.ViewCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

import java.util.List;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivitySummary;
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
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;


    public NormalBottomSheet(Context context, BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior) {
        super(context);
        init(context);

        this.bottomSheetBehavior = bottomSheetBehavior;
    }

    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget, null);

        sessionsRecyclerView = bottomSheet.findViewById(R.id.sessions_recycler_view);

        sessionsAdapter = new SessionsAdapter();
        sessionsRecyclerView.setAdapter(sessionsAdapter);
        sessionsRecyclerView.setLayoutManager(new LinearLayoutManager(context));

        final float maxHeightPx = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 500, context.getResources().getDisplayMetrics());

        sessionsRecyclerView.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                if (sessionsRecyclerView.getHeight() > maxHeightPx) {
                    ViewGroup.LayoutParams params = sessionsRecyclerView.getLayoutParams();
                    params.height = (int) maxHeightPx;
                    sessionsRecyclerView.setLayoutParams(params);
                }
                sessionsRecyclerView.getViewTreeObserver().removeOnGlobalLayoutListener(this);
            }
        });

        summaryRecyclerView = bottomSheet.findViewById(R.id.summary_recycler_view);


        summaryAdapter = new ActivitySummaryAdapter();
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

    public void updateSessionsList(List<Session> sessionList, TrajectorySegment clickedSegment) {
        if (sessionList != null && !sessionList.isEmpty()) {
            sessionsAdapter.setUniqueSessionsList(sessionList, clickedSegment);

            TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
            if (trajectoryTextView != null) {
                trajectoryTextView.setVisibility(View.GONE);
            }
        } else {
            showEmptyState();
        }
    }

    public void updateSummaryView(List<ActivitySummary> summaryActivityItemList) {
        if (summaryActivityItemList != null && !summaryActivityItemList.isEmpty()) {
            summaryAdapter.setActivityItemList(summaryActivityItemList);

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

        if (clickedSegment != null) {
            Log.d("SCROLL_DEBUG", "Scrolling to session: " + clickedSegment.getSessionNumber());

            sessionsRecyclerView.post(() -> {
                LinearLayoutManager layoutManager = (LinearLayoutManager) sessionsRecyclerView.getLayoutManager();
                if (layoutManager != null) {
                    layoutManager.scrollToPositionWithOffset(clickedSegment.getSessionNumber() - 1, 0);
                }
            });
        }

    }

}
