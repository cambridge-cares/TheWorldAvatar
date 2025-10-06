package uk.ac.cam.cares.jps.user;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static android.Manifest.permission.ACTIVITY_RECOGNITION;
import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.RECORD_AUDIO;

import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import dagger.hilt.android.AndroidEntryPoint;

import uk.ac.cam.cares.jps.sensor.permission.PermissionHelper;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.ui.viewmodel.UserAccountViewModel;
import uk.ac.cam.cares.jps.user.databinding.FragmentSensorSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.SensorAdapter;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;

/**
 * Fragment that manages the sensor settings UI and handles user interactions
 * related to starting and stopping sensor recordings.
 */
@AndroidEntryPoint
public class SensorSettingFragment extends Fragment {

    private FragmentSensorSettingBinding binding;
    private SensorViewModel sensorViewModel;
    private UserAccountViewModel accountViewModel;
    private SensorAdapter adapter;

    private final PermissionHelper permissionHelper = new PermissionHelper(this);

    /**
     * Called to have the fragment instantiate its user interface view.
     *
     * @param inflater           The LayoutInflater object that can be used to inflate any views in the fragment.
     * @param container          If non-null, this is the parent view that the fragment's UI should be attached to.
     * @param savedInstanceState If non-null, this fragment is being re-constructed from a previous saved state as given here.
     * @return Return the View for the fragment's UI, or null.
     */
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentSensorSettingBinding.inflate(inflater);
        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);
        accountViewModel = new ViewModelProvider(this).get(UserAccountViewModel.class);
        sensorViewModel.getHasAccountError().observe(getViewLifecycleOwner(), hasAccountError -> {
            if (!hasAccountError) {
                return;
            }
            accountViewModel.getSessionExpiredDialog(this).show();
        });

        return binding.getRoot();
    }


    /**
     * Called immediately after onCreateView has returned, but before any saved state has been restored in to the view.
     *
     * @param view               The View returned by {@link #onCreateView(LayoutInflater, ViewGroup, Bundle)}.
     * @param savedInstanceState If non-null, this fragment is being re-constructed
     *                           from a previous saved state as given here.
     */
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        sensorViewModel.checkRecordingStatusAndUpdateUI();

        setupRecyclerView(view);

        setupObservers();

        binding.startRecordTv.setOnClickListener(view1 -> onRecordButtonClicked(view));

        setupUIInteractions();

    }

    /**
     * Sets up the RecyclerView for displaying sensor items, initializes the adapter,
     * and configures the layout and item decorations for the RecyclerView.
     *
     * @param view The root view of the fragment where the RecyclerView is located.
     */
    private void setupRecyclerView(View view) {
        RecyclerView recyclerView = view.findViewById(R.id.sensors_recycler_view);
        recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        DividerItemDecoration dividerItemDecoration = new DividerItemDecoration(recyclerView.getContext(),
                LinearLayoutManager.VERTICAL);
        recyclerView.addItemDecoration(dividerItemDecoration);

        adapter = new SensorAdapter(new ArrayList<>(), sensorViewModel);
        recyclerView.setAdapter(adapter);
    }

    /**
     * Sets up observers to monitor changes in the sensor items, the toggle-all state, and
     * the recording state. Updates the UI and RecyclerView based on the observed data.
     */
    @SuppressLint("NotifyDataSetChanged")
    private void setupObservers() {
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            binding.startRecordTv.setText(isRecording ? R.string.stop_recording : R.string.start_recording);
            binding.toggleAllBtn.setEnabled(!isRecording);
            adapter.setTogglesEnabled(!isRecording);
            binding.recordingNoteTv.setVisibility(isRecording ? View.VISIBLE : View.GONE);
        });

        adapter.updateSensorItems(sensorViewModel.getSensorItems());
        adapter.notifyDataSetChanged();

        sensorViewModel.getAllToggledOn().observe(getViewLifecycleOwner(), this::updateToggleAllState);
    }



    /**
     * Updates the "Toggle All" button state based on whether all sensors are toggled on.
     *
     * @param isAllToggledOn A Boolean indicating if all sensors are toggled on.
     */
    private void updateToggleAllState(Boolean isAllToggledOn) {
        boolean isRecording = Boolean.TRUE.equals(sensorViewModel.getIsRecording().getValue());
        binding.toggleAllBtn.setText(isAllToggledOn ? R.string.toggle_off : R.string.toggle_all);
        adapter.setTogglesEnabled(!isRecording);
    }


    /**
     * Sets up UI interactions, including the "Toggle All" button functionality and
     * navigation back to the previous screen.
     */
    private void setupUIInteractions() {
        binding.toggleAllBtn.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(sensorViewModel.getIsRecording().getValue())) {
                Toast.makeText(requireContext(), "Stop recording before changing sensor toggles.", Toast.LENGTH_SHORT).show();
                return;
            }

            boolean currentToggleState = sensorViewModel.getAllToggledOn().getValue() != null &&
                    sensorViewModel.getAllToggledOn().getValue();
            sensorViewModel.toggleAllSensors(!currentToggleState);
        });

        binding.topAppbar.setNavigationOnClickListener(view1 ->
                NavHostFragment.findNavController(this).navigateUp());
    }


    /**
     * Handles the logic for starting or stopping sensor recording when the button is clicked.
     *
     * @param view The view that was clicked.
     */
    private void onRecordButtonClicked(View view) {

        if (Boolean.TRUE.equals(sensorViewModel.getIsRecording().getValue())) {
            // Stop recording otherwise itll check permissions when it shldn't
            sensorViewModel.stopRecording();
            binding.startRecordTv.setText(R.string.start_recording);
            return;
        }

        List<SensorType> selectedSensorTypes = sensorViewModel.getSelectedSensors().getValue();

        // Check if there are any sensors selected
        if (selectedSensorTypes == null || selectedSensorTypes.isEmpty()) {
            Toast.makeText(requireContext(), "Please enable at least one sensor or click Toggle All", Toast.LENGTH_SHORT).show();
            return;
        }

        promptGrantPermissions();
        boolean locationToggled = selectedSensorTypes.contains(SensorType.LOCATION);
        boolean audioToggled = selectedSensorTypes.contains(SensorType.SOUND);
        boolean activityToggled = selectedSensorTypes.contains(SensorType.ACTIVITY);

        Runnable startRecordingRunnable = () -> sensorViewModel.toggleRecording();
        List<String> permissions = new ArrayList<>(Arrays.asList(
                locationToggled && needToPermissionGranted(ACCESS_FINE_LOCATION) ? ACCESS_FINE_LOCATION : null,
                audioToggled && needToPermissionGranted(RECORD_AUDIO) ? RECORD_AUDIO : null,
                activityToggled && needToPermissionGranted(ACTIVITY_RECOGNITION) ? ACTIVITY_RECOGNITION : null,
                needToPermissionGranted(POST_NOTIFICATIONS) ? POST_NOTIFICATIONS : null));
        permissions.removeIf(Objects::isNull);
        permissionHelper.requestPermissionsInChain(permissions, startRecordingRunnable);

    }

    private boolean needToPermissionGranted(String permission) {
        return ContextCompat.checkSelfPermission(requireContext(), permission) != PackageManager.PERMISSION_GRANTED;
    }

    /**
     * Prompts the user to enable permissions if the haven't yet.
     */
    private void promptGrantPermissions() {
        List<String> permissionNotGranted = new ArrayList<>();
        List<SensorType> selectedSensorTypes = sensorViewModel.getSelectedSensors().getValue();

        if (needToPermissionGranted(POST_NOTIFICATIONS)) {
            permissionNotGranted.add("Notification");
        }

        if (selectedSensorTypes.contains(SensorType.ACTIVITY) &&
                needToPermissionGranted(ACTIVITY_RECOGNITION)) {
            permissionNotGranted.add("Activity");
        }

        if (selectedSensorTypes.contains(SensorType.LOCATION) &&
                needToPermissionGranted(ACCESS_FINE_LOCATION)) {
            permissionNotGranted.add("Location");
        }

        if (selectedSensorTypes.contains(SensorType.SOUND) &&
                needToPermissionGranted(RECORD_AUDIO)) {
            permissionNotGranted.add("Audio");
        }

        if (!permissionNotGranted.isEmpty()) {
            Toast.makeText(requireContext(), String.join(", ", permissionNotGranted) + " permission(s) not granted. Not able to start recording.", Toast.LENGTH_LONG).show();
        }
    }
}