package uk.ac.cam.cares.jps.user;

import static uk.ac.cam.cares.jps.user.LoginFragment.LOGGER;

import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.os.Bundle;
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

    private void setupRecyclerView(View view) {
        RecyclerView recyclerView = view.findViewById(R.id.sensors_recycler_view);
        recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        DividerItemDecoration dividerItemDecoration = new DividerItemDecoration(recyclerView.getContext(),
                LinearLayoutManager.VERTICAL);
        recyclerView.addItemDecoration(dividerItemDecoration);

        adapter = new SensorAdapter(new ArrayList<>(), sensorViewModel);
        recyclerView.setAdapter(adapter);
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setupObservers() {
        sensorViewModel.getSensorItems().observe(getViewLifecycleOwner(), sensorItems -> {
            adapter.updateSensorItems(sensorItems);
            adapter.notifyDataSetChanged();
        });
        sensorViewModel.getAllToggledOn().observe(getViewLifecycleOwner(), this::updateToggleAllState);
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), this::updateRecordingUI);

    }

    private void updateToggleAllState(Boolean isAllToggledOn) {
        binding.toggleAllBtn.setText(isAllToggledOn ? R.string.toggle_off : R.string.toggle_all);
        adapter.setTogglesEnabled(isAllToggledOn);
    }

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

        // Retrieve selected sensors from ViewModel (which is now responsible for managing the state)
        List<SensorType> selectedSensorTypes = sensorViewModel.getSelectedSensors().getValue();

        // Check if there are any sensors selected
        if (selectedSensorTypes == null || selectedSensorTypes.isEmpty()) {
            Toast.makeText(requireContext(), "Please enable at least one sensor or click Toggle All", Toast.LENGTH_SHORT).show();
            return;
        }

        // Check if location or audio sensors are toggled, and request permissions accordingly
        boolean locationToggled = selectedSensorTypes.contains(SensorType.LOCATION);
        boolean audioToggled = selectedSensorTypes.contains(SensorType.SOUND);

        final boolean[] permissionsGranted = {false, false};


        Runnable startRecordingRunnable = () -> startRecording();
        LOGGER.info("Started recording for sensors: " + selectedSensorTypes);

        if (locationToggled) {
            Permission locationPermission = new Permission(Permission.PermissionType.LOCATION_FINE, requestFineLocationPermissionLauncher);
            checkPermission(locationPermission, () -> {
                if (audioToggled) {
                    LOGGER.info("Requesting audio permission...");
                    Permission audioPermission = new Permission(Permission.PermissionType.AUDIO, requestAudioPermissionLauncher);
                    checkPermission(audioPermission, startRecordingRunnable);
                    LOGGER.info("Requesting audio permission...2");
                } else {
                    startRecordingRunnable.run();
                    LOGGER.info("Not Requesting audio permission...");
                }
            });
        } else if (audioToggled) {
            // If only audio is toggled, request audio permission
            Permission audioPermission = new Permission(Permission.PermissionType.AUDIO, requestAudioPermissionLauncher);
            checkPermission(audioPermission, startRecordingRunnable);
        } else {
            // No permissions needed, start recording immediately
            startRecordingRunnable.run();
        }

    }

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

    private void checkPermission(Permission permission, Runnable onPermissionGranted) {
        if (ContextCompat.checkSelfPermission(requireContext(), permission.permissionString) == PackageManager.PERMISSION_GRANTED) {
            onPermissionGranted.run();
        } else if (ActivityCompat.shouldShowRequestPermissionRationale(requireActivity(), permission.permissionString)) {
            new MaterialAlertDialogBuilder(requireContext())
                    .setMessage(permission.explanation)
                    .setPositiveButton(uk.ac.cam.cares.jps.ui.R.string.ok, (dialogInterface, i)  -> permission.launcher.launch(permission.permissionString)).create().show();
            if (permission.permissionString.isEmpty()) {
                permission.isGranted = true;
                onPermissionGranted.run();
                return;
            }
        } else {
            permission.launcher.launch(permission.permissionString);
        }
    }

    private final ActivityResultLauncher<String> requestFineLocationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {
            startRecording();
        }
    });

    private final ActivityResultLauncher<String> requestAudioPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {
            startRecording();
        }
    });

    private final ActivityResultLauncher<String> requestNotificationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {

        }
    });


}
