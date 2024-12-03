package uk.ac.cam.cares.jps.user;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.text.SpannableStringBuilder;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
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
    private Runnable activityRecognitionPermissionCallback;
    private Runnable notificationPermissionCallback;

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
     * @param view               The View returned by {@link #onCreateView(LayoutInflater, ViewGroup, Bundle)}.
     * @param savedInstanceState If non-null, this fragment is being re-constructed
     *                           from a previous saved state as given here.
     */
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        sensorViewModel.checkRecordingStatusAndUpdateUI(requireContext());

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
        binding.startRecordTv.setEnabled(true);
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording) {
                binding.startRecordTv.setText(R.string.stop_recording);
                binding.toggleAllBtn.setEnabled(false);
                adapter.setTogglesEnabled(false);
            } else {
                binding.startRecordTv.setText(R.string.start_recording);
                binding.toggleAllBtn.setEnabled(true);
                adapter.setTogglesEnabled(true);
            }
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
        binding.toggleAllBtn.setText(isAllToggledOn ? R.string.toggle_off : R.string.toggle_all);
        adapter.setTogglesEnabled(isAllToggledOn);
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
        assert selectedSensorTypes != null;
        boolean locationToggled = selectedSensorTypes.contains(SensorType.LOCATION);
        boolean audioToggled = selectedSensorTypes.contains(SensorType.SOUND);
        boolean activityToggled = selectedSensorTypes.contains(SensorType.ACTIVITY);


        Runnable startRecordingRunnable = () -> sensorViewModel.toggleRecording();

        if (activityToggled) {
            requestActivityRecognitionPermission(() -> {
                requestSensorPermissions(locationToggled, audioToggled, startRecordingRunnable);
            });
        } else {
            requestSensorPermissions(locationToggled, audioToggled, startRecordingRunnable);
        }

    }

    // todo: should redesign this permission request flow
    private void requestSensorPermissions(boolean locationToggled, boolean audioToggled, Runnable onGranted) {
        if (locationToggled && audioToggled) {
            requestLocationPermission(() -> requestAudioPermission(() -> requestAudioPermission(onGranted)));
        } else if (locationToggled) {
            requestLocationPermission(() -> requestNotificationPermission(onGranted));
        } else if (audioToggled) {
            requestAudioPermission(() -> requestNotificationPermission(onGranted));
        } else {
            requestNotificationPermission(onGranted);
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
        }
    }

    /**
     * Prompts the user to enable permissions if the haven't yet.
     */
    private void promptGrantPermissions() {
        StringBuilder permissionNotGranted = new StringBuilder();
        List<SensorType> selectedSensorTypes = sensorViewModel.getSelectedSensors().getValue();

        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Notification ");
        }

        if (selectedSensorTypes.contains(SensorType.ACTIVITY) &&
                ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACTIVITY_RECOGNITION) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Activity ");
        }

        if (selectedSensorTypes.contains(SensorType.LOCATION) &&
                ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Location ");
        }

        if (selectedSensorTypes.contains(SensorType.SOUND) &&
                ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            permissionNotGranted.append("Audio ");
        }

        if (permissionNotGranted.length() > 0) {
            Toast.makeText(requireContext(), permissionNotGranted.length() + "permission(s) not granted. Not able to start recording.", Toast.LENGTH_LONG).show();
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
            locationPermissionCallback = onGranted;
            requestFineLocationPermissionLauncher.launch(android.Manifest.permission.ACCESS_FINE_LOCATION);
        }
    }

    private void requestActivityRecognitionPermission(Runnable onGranted) {
        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACTIVITY_RECOGNITION) == PackageManager.PERMISSION_GRANTED) {
            onGranted.run();
        } else {
            activityRecognitionPermissionCallback = onGranted;
            requestActivityRecognitionPermissionLauncher.launch(Manifest.permission.ACTIVITY_RECOGNITION);
        }
    }

    private void requestNotificationPermission(Runnable onGranted) {
        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
            onGranted.run();
        } else {
            notificationPermissionCallback = onGranted;
            requestNotificationPermissionLauncher.launch(Manifest.permission.POST_NOTIFICATIONS);
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
            audioPermissionCallback = onGranted;
            requestAudioPermissionLauncher.launch(android.Manifest.permission.RECORD_AUDIO);
        }
    }

    private final ActivityResultLauncher<String> requestFineLocationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && locationPermissionCallback != null) {
            locationPermissionCallback.run();
        }
    });

    private final ActivityResultLauncher<String> requestActivityRecognitionPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && activityRecognitionPermissionCallback != null) {
            activityRecognitionPermissionCallback.run();
        } else {
            Toast.makeText(requireContext(), "Activity Recognition permission is required to start recording.", Toast.LENGTH_SHORT).show();
        }
    });

    private final ActivityResultLauncher<String> requestAudioPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && audioPermissionCallback != null) {
            audioPermissionCallback.run();
        }
    });

    private final ActivityResultLauncher<String> requestNotificationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted && notificationPermissionCallback != null) {
            notificationPermissionCallback.run();
        }
    });

}
