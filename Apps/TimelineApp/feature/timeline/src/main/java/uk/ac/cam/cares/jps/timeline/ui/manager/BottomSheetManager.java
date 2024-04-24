package uk.ac.cam.cares.jps.timeline.ui.manager;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;

import uk.ac.cam.cares.jps.timeline.viewmodel.ConnectionViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.BottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.NormalBottomSheet;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * A class to determine what bottom sheet to show and switch in between different bottom sheet
 * based on the state.
 */
public class BottomSheetManager {
    private TrajectoryViewModel trajectoryViewModel;
    private ConnectionViewModel connectionViewModel;

    private BottomSheet bottomSheet;
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private LinearLayoutCompat bottomSheetContainer;

    private LifecycleOwner lifecycleOwner;
    private Context context;

    private NormalBottomSheet normalBottomSheet;
    private ErrorBottomSheet errorBottomSheet;

    public BottomSheetManager(Fragment fragment, LinearLayoutCompat bottomSheetContainer) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);
        connectionViewModel = new ViewModelProvider(fragment).get(ConnectionViewModel.class);

        lifecycleOwner = fragment.getViewLifecycleOwner();
        context = fragment.requireContext();

        this.bottomSheetContainer = bottomSheetContainer;
        this.bottomSheetBehavior = BottomSheetBehavior.from(bottomSheetContainer);
        initBottomSheet();
    }

    public void initBottomSheet() {
        initNormalBottomSheet();
        initErrorBottomSheet();

        connectionViewModel.getHasConnection().observe(lifecycleOwner, hasConnection -> {
            if (hasConnection) {
                setBottomSheet(normalBottomSheet);
                trajectoryViewModel.getTrajectory();
            } else {
                errorBottomSheet.setErrorMessage(ErrorBottomSheet.ErrorType.CONNECTION_ERROR);
                setAndExtendBottomSheet(errorBottomSheet);
            }
        });
        connectionViewModel.checkNetworkConnection();
    }

    private void initNormalBottomSheet() {
        normalBottomSheet = new NormalBottomSheet(context);

        trajectoryViewModel.trajectoryError.observe(lifecycleOwner, error -> {
            if (error.equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found))) {
                ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
              return;
            }
            errorBottomSheet.setErrorMessage(ErrorBottomSheet.ErrorType.TRAJECTORY_ERROR);
            setAndExtendBottomSheet(errorBottomSheet);
        });

        trajectoryViewModel.isFetchingTrajecjtory.observe(lifecycleOwner, isFetching -> {
            if (isFetching) {
                normalBottomSheet.getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.VISIBLE);
                normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.GONE);


            } else {
                normalBottomSheet.getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.GONE);
                normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.VISIBLE);
            }
        });

        trajectoryViewModel.trajectory.observe(lifecycleOwner, trajectory -> {
            ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(R.string.more_information_about_the_trajectory_will_be_shown_here);
        });
    }

    private void initErrorBottomSheet() {
        View.OnClickListener retryConnectionAndRetrieveTrajectory = view -> {
            connectionViewModel.checkNetworkConnection();
        };

        errorBottomSheet = new ErrorBottomSheet(context, retryConnectionAndRetrieveTrajectory);
    }

    private void setBottomSheet(BottomSheet bottomSheet) {
        bottomSheetContainer.removeAllViews();
        bottomSheetContainer.addView(bottomSheet.getBottomSheet(), MATCH_PARENT, MATCH_PARENT);

        this.bottomSheet = bottomSheet;
    }

    private void setAndExtendBottomSheet(BottomSheet bottomSheet) {
        setBottomSheet(bottomSheet);
        if (bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_EXPANDED &&
                bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_DRAGGING &&
                bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_SETTLING) {
            bottomSheetBehavior.setState(BottomSheetBehavior.STATE_HALF_EXPANDED);
        }
    }


//    private void setupBottomSheet() {
//        bottomSheetBehavior.addBottomSheetCallback(new BottomSheetBehavior.BottomSheetCallback() {
//            @Override
//            public void onStateChanged(@NonNull View view, int i) {
//                LOGGER.info("State is " + i);
//                scaleBarPlugin.updateSettings(scaleBarSettings -> {
//                    scaleBarSettings.setMarginTop(view.getTop() - MAP_BOTTOM_FLOATING_COMPONENT_MARGIN + binding.appBarLayout.getHeight());
//                    return null;
//                });
//
//                if (i == BottomSheetBehavior.STATE_EXPANDED) {
//                    // todo: change the app bar style
//                    TypedValue typedValue = new TypedValue();
//                    requireContext().getTheme().resolveAttribute(androidx.appcompat.R.attr.colorPrimary, typedValue, true);
//
//                    binding.appBarLayout.setBackground(getResources().getDrawable(typedValue.resourceId, requireContext().getTheme()));
//                } else {
//                    binding.appBarLayout.setBackground(getResources().getDrawable(R.drawable.app_bar_background, requireContext().getTheme()));
//                }
//            }
//
//            @Override
//            public void onSlide(@NonNull View view, float v) {
//                scaleBarPlugin.updateSettings(scaleBarSettings -> {
//                    scaleBarSettings.setMarginTop(view.getTop() - MAP_BOTTOM_FLOATING_COMPONENT_MARGIN + binding.appBarLayout.getHeight());
//                    return null;
//                });
//            }
//        });
//    }
}
