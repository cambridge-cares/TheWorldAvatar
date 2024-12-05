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
        if (trajectory.isEmpty()) {
            ((TextView) getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
            return;
        }
        ((TextView) getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(R.string.more_information_about_the_trajectory_will_be_shown_here);
    }
}
