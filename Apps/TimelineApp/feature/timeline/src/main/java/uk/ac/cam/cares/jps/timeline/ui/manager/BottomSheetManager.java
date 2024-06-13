package uk.ac.cam.cares.jps.timeline.ui.manager;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.net.Uri;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.datepicker.MaterialDatePicker;

import org.apache.log4j.Logger;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.BottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.NormalBottomSheet;
import uk.ac.cam.cares.jps.timeline.viewmodel.ConnectionViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.NormalBottomSheetViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.UserPhoneViewModel;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * A class to determine what bottom sheet to show and switch in between different bottom sheet
 * based on the state.
 */
public class BottomSheetManager {
    private final TrajectoryViewModel trajectoryViewModel;
    private final ConnectionViewModel connectionViewModel;
    private final UserPhoneViewModel userPhoneViewModel;
    private final NormalBottomSheetViewModel normalBottomSheetViewModel;

    private Logger LOGGER = Logger.getLogger(BottomSheetManager.class);

    private MaterialDatePicker<Long> datePicker;
    private FragmentManager fragmentManager;
    private final BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private final LinearLayoutCompat bottomSheetContainer;

    private final LifecycleOwner lifecycleOwner;
    private final Context context;
    private final NavController navController;

    private NormalBottomSheet normalBottomSheet;
    private ErrorBottomSheet errorBottomSheet;

    public BottomSheetManager(Fragment fragment, LinearLayoutCompat bottomSheetContainer) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);
        connectionViewModel = new ViewModelProvider(fragment).get(ConnectionViewModel.class);
        userPhoneViewModel = new ViewModelProvider(fragment).get(UserPhoneViewModel.class);
        normalBottomSheetViewModel = new ViewModelProvider(fragment).get(NormalBottomSheetViewModel.class);

        lifecycleOwner = fragment.getViewLifecycleOwner();
        context = fragment.requireContext();
        navController = NavHostFragment.findNavController(fragment);

        datePicker = MaterialDatePicker.Builder.datePicker()
                .setTitleText(R.string.select_date)
                .build();
        datePicker.addOnPositiveButtonClickListener(o -> normalBottomSheetViewModel.setDate(Instant.ofEpochMilli(o).atZone(ZoneId.of("UTC")).toLocalDate()));

        fragmentManager = fragment.getParentFragmentManager();

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
                trajectoryViewModel.getTrajectory(convertDateFormat(normalBottomSheetViewModel.selectedDate.getValue()));
            } else {
                errorBottomSheet.setErrorMessage(ErrorBottomSheet.ErrorType.CONNECTION_ERROR);
                setAndExtendBottomSheet(errorBottomSheet);
            }
        });
        connectionViewModel.checkNetworkConnection();
    }

    private void initNormalBottomSheet() {
        normalBottomSheet = new NormalBottomSheet(context);

        normalBottomSheet.getBottomSheet().findViewById(R.id.date_left_bt).setOnClickListener(view -> normalBottomSheetViewModel.setToLastDate());

        normalBottomSheet.getBottomSheet().findViewById(R.id.date_right_bt).setOnClickListener(view -> normalBottomSheetViewModel.setToNextDate());

        normalBottomSheet.getBottomSheet().findViewById(R.id.date_picker_layout).setOnClickListener(view -> datePicker.show(fragmentManager, "date_picker"));

        normalBottomSheetViewModel.selectedDate.observe(lifecycleOwner, selectedDate -> {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("E, MMMM dd, yyyy");
            ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.date_tv)).setText(selectedDate.format(formatter));
            connectionViewModel.checkNetworkConnection();
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
            if (trajectory.isEmpty()) {
                ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
                return;
            }
            ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.trajectory_info_tv)).setText(R.string.more_information_about_the_trajectory_will_be_shown_here);
        });
    }

    private void initErrorBottomSheet() {
        View.OnClickListener retryConnectionAndRetrieveTrajectory = view -> {
            connectionViewModel.checkNetworkConnection();
        };
        View.OnClickListener registerUserToPhone = view -> {
            userPhoneViewModel.registerPhoneToUser();
        };

        Map<ErrorBottomSheet.ErrorType, View.OnClickListener> errorHandler = new HashMap<>();
        errorHandler.put(ErrorBottomSheet.ErrorType.CONNECTION_ERROR, retryConnectionAndRetrieveTrajectory);
        errorHandler.put(ErrorBottomSheet.ErrorType.ACCOUNT_ERROR, registerUserToPhone);

        errorBottomSheet = new ErrorBottomSheet(context, errorHandler);

        userPhoneViewModel.getHasAccountError().observe(lifecycleOwner, hasAccountError -> {
            if (hasAccountError) {
                // todo: login again?
                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse(context.getString(uk.ac.cam.cares.jps.utils.R.string.login_fragment_link)))
                        .build();
                navController.navigate(request);
            } else {
                // retry getting trajectory
                LOGGER.info("Phone register, retrying to get trajectory");
                connectionViewModel.checkNetworkConnection();
            }
        });

        trajectoryViewModel.trajectoryError.observe(lifecycleOwner, error -> {
            if (error.getMessage() != null &&
                    error.getMessage().equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user))) {
                errorBottomSheet.setErrorMessage(ErrorBottomSheet.ErrorType.ACCOUNT_ERROR);
            } else {
                errorBottomSheet.setErrorMessage(ErrorBottomSheet.ErrorType.TRAJECTORY_ERROR);
            }
            setAndExtendBottomSheet(errorBottomSheet);
        });
    }

    private void setBottomSheet(BottomSheet bottomSheet) {
        bottomSheetContainer.removeAllViews();
        bottomSheetContainer.addView(bottomSheet.getBottomSheet(), MATCH_PARENT, MATCH_PARENT);
    }

    private void setAndExtendBottomSheet(BottomSheet bottomSheet) {
        setBottomSheet(bottomSheet);
        if (bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_EXPANDED &&
                bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_DRAGGING &&
                bottomSheetBehavior.getState() != BottomSheetBehavior.STATE_SETTLING) {
            bottomSheetBehavior.setState(BottomSheetBehavior.STATE_HALF_EXPANDED);
        }
    }

    private String convertDateFormat(LocalDate date) {
        ZonedDateTime convertedDateStart = date.atStartOfDay(ZoneId.systemDefault()).withZoneSameInstant(ZoneId.of("UTC"));
        return convertedDateStart.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSx"));
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
