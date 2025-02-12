

package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;

import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * Bottom sheet to be shown when there is no error
 */
public class NormalBottomSheet extends BottomSheet {

    /**
     * Constructor of the class
     *
     * @param context fragment context
     */
    public NormalBottomSheet(@NonNull Context context) {
        super(context);
    }

    /**
     * Inflate ui
     *
     * @param context fragment context
     */
    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget, null);
    }

    public void showFetchingAnimation(boolean isFetching) {
        if (isFetching) {
            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.VISIBLE);
            getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.GONE);
        } else {
            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.GONE);
            getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.VISIBLE);
        }
    }

    public void showTrajectoryInfo(String trajectory) {
        // Check if the trajectory data is empty
        if (trajectory.isEmpty()) {
            ((TextView) getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
            updateSummary("No summary available as trajectory data is empty.");
            return;
        }

        updateSummary("Activity Summary: \n" + trajectory); 
    }

    /**
     * Update the activity summary text in the bottom sheet
     * 
     * @param summary the summary of activity as text
     */
    public void updateSummary(String summary) {
        ((TextView) getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(summary);
    }
}
