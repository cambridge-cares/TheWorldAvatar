package uk.ac.cam.cares.jps.user;

import static uk.ac.cam.cares.jps.user.LoginFragment.LOGGER;

import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;
import androidx.recyclerview.widget.DividerItemDecoration;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.user.databinding.FragmentSensorSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
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
    private AccountViewModel accountViewModel;
    private SensorAdapter adapter;
    private Runnable locationPermissionCallback;
    private Runnable audioPermissionCallback;




    /**
     * Called to have the fragment instantiate its user interface view.
     *
     * @param inflater The LayoutInflater object that can be used to inflate any views in the fragment.
     * @param container If non-null, this is the parent view that the fragment's UI should be attached to.
     * @param savedInstanceState If non-null, this fragment is being re-constructed from a previous saved state as given here.
     * @return Return the View for the fragment's UI, or null.
     */
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentSensorSettingBinding.inflate(inflater);
        sensorViewModel = new ViewModelProvider(this).get(SensorViewModel.class);
        accountViewModel = new ViewModelProvider(this).get(AccountViewModel.class);
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
     * @param view The View returned by {@link #onCreateView(LayoutInflater, ViewGroup, Bundle)}.
     * @param savedInstanceState If non-null, this fragment is being re-constructed
     * from a previous saved state as given here.
     */
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        sensorViewModel.checkRecordingStatusAndUpdateUI(requireContext());

        setupRecyclerView(view);

        binding.startRecordTv.setEnabled(true);
        binding.startRecordTv.setOnClickListener(this::onRecordButtonClicked);

        setupObservers();

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
        sensorViewModel.getSensorItems().observe(getViewLifecycleOwner(), sensorItems -> {
            adapter.updateSensorItems(sensorItems);
            adapter.notifyDataSetChanged();
        });
        sensorViewModel.getAllToggledOn().observe(getViewLifecycleOwner(), this::updateToggleAllState);
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), this::updateRecordingUI);

    }

    /**
     * Updates the "Toggle All" button state based on whether all sensors are toggled on.
     *
     * @param isAllToggledOn A Boolean indicating if all sensors are toggled on.
     */
    private void updateToggleAllState(Boolean isAllToggledOn) {
        binding.toggleAllBtn.setText(isAllToggledOn ? R.string.toggle_off : R.string.toggle_all);
        adapter.setTogglesEnabled(isAllToggledOn);
    }

    /**
     * Updates the UI to reflect the current recording state.
     * Disables toggling sensors while recording and re-enables it when recording stops.
     *
     * @param isRecording A Boolean indicating whether sensor recording is active.
     */
    @SuppressLint("NotifyDataSetChanged")
    private void updateRecordingUI(Boolean isRecording) {
        binding.startRecordTv.setText(isRecording ? R.string.stop_recording : R.string.start_recording);
        adapter.setTogglesEnabled(!isRecording);
        binding.toggleAllBtn.setEnabled(!isRecording);

        if (!isRecording) {
            sensorViewModel.toggleAllSensors(false);
            adapter.notifyDataSetChanged();
        }
    }

    /**
     * Sets up UI interactions, including the "Toggle All" button functionality and
     * navigation back to the previous screen.
     */
    private void setupUIInteractions() {
        binding.toggleAllBtn.setOnClickListener(v -> {
            boolean currentToggleState = sensorViewModel.getAllToggledOn().getValue() != null && sensorViewModel.getAllToggledOn().getValue();
            sensorViewModel.toggleAllSensors(!currentToggleState);
        });

        binding.topAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());
    }


    /**
     * Handles the logic for starting or stopping sensor recording when the button is clicked.
     *
     * @param view The view that was clicked.
     */
    private void onRecordButtonClicked(View view) {
        LOGGER.info("recording button clicked");

        if (Boolean.TRUE.equals(sensorViewModel.getIsRecording().getValue())) {
            // Stop recording otherwise itll check permissions when it shldn't
            sensorViewModel.stopRecording();
            binding.startRecordTv.setText(R.string.start_recording);
            return;
        }

        List<SensorType> selectedSensorTypes = sensorViewModel.getSelectedSensors().getValue();

        // Check if there are any sensors selected
        promptSelectSensors(selectedSensorTypes);
        promptGrantPermissions();

        // Check if location or audio sensors are toggled, and request permissions accordingly
        boolean locationToggled = selectedSensorTypes.contains(SensorType.LOCATION);
        boolean audioToggled = selectedSensorTypes.contains(SensorType.SOUND);

        Runnable startRecordingRunnable = this::startRecording;

        if (locationToggled && audioToggled) {
            // Request location first, then audio
            requestLocationPermission(() -> requestAudioPermission(startRecordingRunnable));
        } else if (locationToggled) {
            requestLocationPermission(startRecordingRunnable);
        } else if (audioToggled) {
            requestAudioPermission(startRecordingRunnable);
        } else {
            startRecording();
        }

    }

    /**
     * Prompts the user to toggle on a sensor if they haven't yet.
     *
     * @param selectedSensorTypes list of sensors the user has selected.
     *                            Empty if the user has not selected any sensors.
     */
    private void promptSelectSensors(List<SensorType> selectedSensorTypes) {
        if (selectedSensorTypes == null || selectedSensorTypes.isEmpty()) {
            Toast.makeText(requireContext(), "Please enable at least one sensor or click Toggle All", Toast.LENGTH_SHORT).show();
            return;
        }
    }

    /**
     * Prompts the user to enable permissions if the haven't yet.
     */
    private void promptGrantPermissions() {
        StringBuilder permissionNotGranted = new StringBuilder();

        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Location ");
        }

        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Audio ");
        }

        if (permissionNotGranted.length() > 0) {
            Toast.makeText(requireContext(), permissionNotGranted.toString() + "permission(s) not granted. Not able to start recording.", Toast.LENGTH_LONG).show();
        }
    }

    /**
     * Starts or stops recording based on the current recording state.
     * If recording is active, stops it and resets all sensors.
     * If recording is not active, starts recording only for the selected sensors.
     */
    @SuppressLint("NotifyDataSetChanged")
    private void startRecording() {
        Boolean isRecording = sensorViewModel.getIsRecording().getValue();
        if (isRecording != null && isRecording) {
            // Stop recording and reset sensors
            sensorViewModel.stopRecording();
            sensorViewModel.toggleAllSensors(false);
            adapter.setTogglesEnabled(true);
            adapter.notifyDataSetChanged();
            binding.toggleAllBtn.setEnabled(true);
        } else {
            // Start recording only the selected sensors
            sensorViewModel.startRecording();
            adapter.setTogglesEnabled(false);
            binding.toggleAllBtn.setEnabled(false);
        }
    }

    /**
     * Requests fine location permission if it hasn't been granted. If granted, runs the provided callback.
     *
     * @param onGranted A {@link Runnable} that is executed if the permission is granted.
     */
    private void requestLocationPermission(Runnable onGranted) {
        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            onGranted.run();
        } else {
            requestFineLocationPermissionLauncher.launch(android.Manifest.permission.ACCESS_FINE_LOCATION);
            locationPermissionCallback  = onGranted;
        }
    }

    /**
     * Requests audio recording permission if it hasn't been granted. If granted, runs the provided callback.
     *
     * @param onGranted A {@link Runnable} that is executed if the permission is granted.
     */
    private void requestAudioPermission(Runnable onGranted) {
        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED) {
            onGranted.run();
        } else {
            requestAudioPermissionLauncher.launch(android.Manifest.permission.RECORD_AUDIO);
            audioPermissionCallback = onGranted;
        }
    }


    private final ActivityResultLauncher<String> requestFineLocationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && locationPermissionCallback != null) {
            locationPermissionCallback.run();
            locationPermissionCallback = null;
        }
    });

    private final ActivityResultLauncher<String> requestAudioPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && audioPermissionCallback != null) {
            audioPermissionCallback.run();
            audioPermissionCallback = null;
        }
    });

    private final ActivityResultLauncher<String> requestNotificationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {

        }
    });


}
