package uk.ac.cam.cares.jps.timeline.ui.manager;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.datepicker.MaterialDatePicker;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.Logger;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateException;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.SummaryActivityItem;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.BottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.NormalBottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.datepicker.GreyOutDecorator;
import uk.ac.cam.cares.jps.timeline.viewmodel.ConnectionViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.NormalBottomSheetViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.UserPhoneViewModel;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * An UI manager that manages bottom sheets on screen and switches in between different bottom sheets
 * based on the state.
 */
public class BottomSheetManager {
    private final TrajectoryViewModel trajectoryViewModel;
    private final ConnectionViewModel connectionViewModel;
    private final UserPhoneViewModel userPhoneViewModel;
    private final NormalBottomSheetViewModel normalBottomSheetViewModel;

    private Logger LOGGER = Logger.getLogger(BottomSheetManager.class);

    private FragmentManager fragmentManager;
    private final BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private final LinearLayoutCompat bottomSheetContainer;

    private final LifecycleOwner lifecycleOwner;
    private final Context context;

    private NormalBottomSheet normalBottomSheet;
    private ErrorBottomSheet errorBottomSheet;
    private final MaterialAlertDialogBuilder sessionExpiredDialog;
    private GreyOutDecorator greyOutDecorator;

    /**
     * Constructor of the class
     * @param fragment Fragment that hosts the bottom sheet
     * @param bottomSheetContainer Container of the bottom sheet
     */
    public BottomSheetManager(Fragment fragment, LinearLayoutCompat bottomSheetContainer) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);
        connectionViewModel = new ViewModelProvider(fragment).get(ConnectionViewModel.class);
        userPhoneViewModel = new ViewModelProvider(fragment).get(UserPhoneViewModel.class);
        normalBottomSheetViewModel = new ViewModelProvider(fragment).get(NormalBottomSheetViewModel.class);

        lifecycleOwner = fragment.getViewLifecycleOwner();
        context = fragment.requireContext();

        fragmentManager = fragment.getParentFragmentManager();
        sessionExpiredDialog = userPhoneViewModel.getSessionExpiredDialog(fragment);

        this.bottomSheetContainer = bottomSheetContainer;
        this.bottomSheetBehavior = BottomSheetBehavior.from(bottomSheetContainer);
        greyOutDecorator = new GreyOutDecorator();
        initBottomSheet();
    }

    private void initBottomSheet() {
        initNormalBottomSheet();
        initErrorBottomSheet();

        connectionViewModel.getHasConnection().observe(lifecycleOwner, hasConnection -> {
            if (hasConnection) {
                setBottomSheet(normalBottomSheet);
                trajectoryViewModel.getTrajectory(normalBottomSheetViewModel.selectedDate.getValue());
            } else {
                errorBottomSheet.setErrorType(ErrorBottomSheet.ErrorType.CONNECTION_ERROR);
                setAndExtendBottomSheet(errorBottomSheet);
            }
        });
        connectionViewModel.checkNetworkConnection();
    }

    private void initNormalBottomSheet() {
        normalBottomSheet = new NormalBottomSheet(context);
        configureDateSelection();
        configureTrajectoryRetrieval();
        configureSummary();
    }

    private void configureTrajectoryRetrieval() {
        trajectoryViewModel.isFetchingTrajectory.observe(lifecycleOwner, normalBottomSheet::showFetchingAnimation);
    }

    private void configureSummary() {

        trajectoryViewModel.trajectory.observe(lifecycleOwner, normalBottomSheetViewModel::parseSessionSummaries);

        normalBottomSheetViewModel.sessionSummary.observe(lifecycleOwner, sessionSummaryByDate -> {
            List<SummaryActivityItem> activityItemSummaryList = sessionSummaryByDate.getActivitySummary();
            List<Session> uniqueSessions = sessionSummaryByDate.getUniqueSessions();
            if(sessionSummaryByDate.getDate().equals(normalBottomSheetViewModel.selectedDate.getValue())) {
                normalBottomSheet.updateSummaryView(activityItemSummaryList);
                normalBottomSheet.updateUniqueSessionsList(uniqueSessions);
            }

        });
    }

    private void configureDateSelection() {
        normalBottomSheet.getBottomSheet().findViewById(R.id.date_left_bt).setOnClickListener(view ->
                normalBottomSheetViewModel.setToLastDate());

        normalBottomSheet.getBottomSheet().findViewById(R.id.date_right_bt).setOnClickListener(view ->
                normalBottomSheetViewModel.setToNextDate());

        normalBottomSheet.getBottomSheet().findViewById(R.id.date_picker_layout).setOnClickListener(view ->
                getDatePicker(normalBottomSheetViewModel).show(fragmentManager, "date_picker"));

        normalBottomSheetViewModel.selectedDate.observe(lifecycleOwner, selectedDate -> {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("E, MMMM dd, yyyy");
            ((TextView) normalBottomSheet.getBottomSheet().findViewById(R.id.date_tv)).setText(selectedDate.format(formatter));
            connectionViewModel.checkNetworkConnection();
        });
        normalBottomSheetViewModel.datesWithTrajectory.observe(lifecycleOwner, dates -> greyOutDecorator.setDatesWithTrajectory(dates));
        normalBottomSheetViewModel.getDatesWithTrajectory(ZonedDateTime.now(ZoneId.systemDefault()).toOffsetDateTime().getOffset().getId());
    }

    private MaterialDatePicker<Long> getDatePicker(NormalBottomSheetViewModel normalBottomSheetViewModel) {
        MaterialDatePicker<Long> datePicker = MaterialDatePicker.Builder.datePicker()
                .setTitleText(R.string.select_date)
                .setSelection(normalBottomSheetViewModel.getSelectedDateLong())
                .setDayViewDecorator(greyOutDecorator)
                .build();
        datePicker.addOnPositiveButtonClickListener(o -> normalBottomSheetViewModel.setDate(Instant.ofEpochMilli(o).atZone(ZoneId.of("UTC")).toLocalDate()));
        return datePicker;
    }

    private void initErrorBottomSheet() {
        errorBottomSheet = new ErrorBottomSheet(context, connectionViewModel, userPhoneViewModel);
        userPhoneViewModel.getError().observe(lifecycleOwner, error -> {
            if (error == null) {
                return;
            }

            if (error instanceof AccountException) {
                // session expired
                sessionExpiredDialog.show();
            } else if (error instanceof SensorCollectionStateException) {
                // retry getting user id, device id and registration
                errorBottomSheet.setErrorType(ErrorBottomSheet.ErrorType.ACCOUNT_ERROR);
            }
        });

        trajectoryViewModel.trajectoryError.observe(lifecycleOwner, error -> {
            if (error == null) {
                return;
            }

            if (error instanceof AccountException) {
                // retry register phone to user
                errorBottomSheet.setErrorType(ErrorBottomSheet.ErrorType.ACCOUNT_ERROR);
            } else {
                LOGGER.error("error in trajectory retrieval: " + error.getMessage(), error);
                errorBottomSheet.setErrorType(ErrorBottomSheet.ErrorType.TRAJECTORY_ERROR);
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

}

