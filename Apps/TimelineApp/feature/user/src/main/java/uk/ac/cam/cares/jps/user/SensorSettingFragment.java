package uk.ac.cam.cares.jps.user;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.SensorItem;
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
public class SensorSettingFragment extends Fragment implements OnSensorToggleListener {

    private FragmentSensorSettingBinding binding;
    private SensorViewModel sensorViewModel;
    private AccountViewModel accountViewModel;
    private Map<Permission.PermissionType, Permission> permissionsMap = new HashMap<>();
    private List<Permission.PermissionType> criticalPermissionType = Arrays.asList(Permission.PermissionType.LOCATION_FINE, Permission.PermissionType.AUDIO);
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
        initPermissions();

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

        RecyclerView recyclerView = view.findViewById(R.id.sensors_recycler_view);
        recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));

        DividerItemDecoration dividerItemDecoration = new DividerItemDecoration(recyclerView.getContext(),
                LinearLayoutManager.VERTICAL);
        recyclerView.addItemDecoration(dividerItemDecoration);


        // Initialize the adapter with an empty list first
        adapter = new SensorAdapter(new ArrayList<>(), this, sensorViewModel);
        recyclerView.setAdapter(adapter);


        sensorViewModel.getSensorItems().observe(getViewLifecycleOwner(), sensorItems -> {
            adapter.updateSensorItems(sensorItems);
            adapter.notifyDataSetChanged();
        });

        Button toggleAllBtn = view.findViewById(R.id.toggle_all_btn);
        sensorViewModel.getAllToggledOn().observe(getViewLifecycleOwner(), isAllToggledOn -> {
            // Update all the sensor items in the adapter to reflect the "Toggle All" state
            for (SensorItem item : adapter.getSensorItems()) {
                item.setToggled(isAllToggledOn);
            }
            adapter.notifyDataSetChanged();

            binding.toggleAllBtn.setText(isAllToggledOn ? R.string.toggle_off : R.string.toggle_all);

            updateStartRecordingButtonState();
        });

        toggleAllBtn.setOnClickListener(v -> {
            boolean currentToggleState = sensorViewModel.getAllToggledOn().getValue() != null && sensorViewModel.getAllToggledOn().getValue();
            sensorViewModel.toggleAllSensors(!currentToggleState);  // Toggle the value
        });


        binding.topAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());

        binding.startRecordTv.setOnClickListener(this::onRecordButtonClicked);
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording) {
                binding.startRecordTv.setText(R.string.stop_recording);
                adapter.setTogglesEnabled(false);
                binding.toggleAllBtn.setEnabled(false);
            } else {
                binding.startRecordTv.setText(R.string.start_recording);
                adapter.setTogglesEnabled(true);
                updateStartRecordingButtonState();
                resetToggleAllButton();
                binding.toggleAllBtn.setEnabled(true);
            }
        });
    }

    /**
     * Resets the state of the "Toggle All" button to its default state.
     */
    private void resetToggleAllButton() {
        binding.toggleAllBtn.setText(R.string.toggle_all);
    }

    /**
     * Updates the state of the "Start Recording" button based on the current sensor toggle states.
     */
    private void updateStartRecordingButtonState() {
        // enable the start recording button only if at least one sensor is toggled on
        boolean hasToggledOnSensor = false;
        for (SensorItem item : adapter.getSensorItems()) {
            if (item.isToggled()) {
                hasToggledOnSensor = true;
                break;
            }
        }
        binding.startRecordTv.setEnabled(hasToggledOnSensor);
    }

    /**
     * Handles the logic for starting or stopping sensor recording when the button is clicked.
     *
     * @param view The view that was clicked.
     */
    private void onRecordButtonClicked(View view) {
        // check permission
        for (Permission permission : permissionsMap.values()) {
            checkFineLocationPermission(permission);
        }

        String permissionNotGranted = permissionsMap.values().stream()
                .filter(permission -> criticalPermissionType.contains(permission.type) && !permission.isGranted)
                .map(permission -> permission.type.toString())
                .collect(Collectors.joining(" "));
        if (!permissionNotGranted.isEmpty()) {
            Toast.makeText(requireContext(), permissionNotGranted + " not granted. Not able to start recording.", Toast.LENGTH_LONG).show();
            return;
        }

        // Gather selected sensor types
        List<SensorItem> selectedSensorTypes = new ArrayList<>();
        for (SensorItem item : adapter.getSensorItems()) {
            if (item.isToggled()) {
                selectedSensorTypes.add(item);
            }
        }


        sensorViewModel.isRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording) {
                binding.startRecordTv.setText(R.string.stop_recording);
                adapter.setTogglesEnabled(false);
                binding.toggleAllBtn.setEnabled(false);
            } else {
                binding.startRecordTv.setText(R.string.start_recording);
                adapter.setTogglesEnabled(true);
                binding.toggleAllBtn.setEnabled(true);
            }
        });

        // Start/Stop recording
        binding.startRecordTv.setOnClickListener(v -> {
            if (Boolean.TRUE.equals(sensorViewModel.isRecording().getValue())) {
                sensorViewModel.stopRecording();
                sensorViewModel.toggleAllSensors(false);
            } else {
                sensorViewModel.startRecording();
            }
        });

        // Toggle all sensors
        binding.toggleAllBtn.setOnClickListener(v -> {
            boolean toggleAll = !sensorViewModel.getAllToggledOn().getValue();
            sensorViewModel.toggleAllSensors(toggleAll);
        });
    }

    private void initPermissions() {
        permissionsMap.put(Permission.PermissionType.LOCATION_FINE, new Permission(Permission.PermissionType.LOCATION_FINE, requestFineLocationPermissionLauncher));
        permissionsMap.put(Permission.PermissionType.AUDIO, new Permission(Permission.PermissionType.AUDIO, requestAudioPermissionLauncher));
        permissionsMap.put(Permission.PermissionType.NOTIFICATION, new Permission(Permission.PermissionType.NOTIFICATION, requestNotificationPermissionLauncher));
    }


    private void checkFineLocationPermission(Permission permission) {
        if (ContextCompat.checkSelfPermission(requireContext(), permission.permissionString) == PackageManager.PERMISSION_GRANTED) {
            permission.isGranted = true;
        } else if (ActivityCompat.shouldShowRequestPermissionRationale(requireActivity(), permission.permissionString)) {
            new MaterialAlertDialogBuilder(requireContext())
                    .setMessage(permission.explanation)
                    .setPositiveButton(uk.ac.cam.cares.jps.ui.R.string.ok, (dialogInterface, i) -> {
                        // some permissions (eg. notification) may be auto granted in lower sdk version
                        if (permission.permissionString.isEmpty()) {
                            permission.isGranted = true;
                            return;
                        }
                        permission.launcher.launch(permission.permissionString);
                    })
                    .create().show();
        } else {
            if (permission.permissionString.isEmpty()) {
                permission.isGranted = true;
                return;
            }
            permission.launcher.launch(permission.permissionString);
        }
    }

    private final ActivityResultLauncher<String> requestFineLocationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {
            permissionsMap.get(Permission.PermissionType.LOCATION_FINE).isGranted = true;
        }
    });

    private final ActivityResultLauncher<String> requestAudioPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {
            permissionsMap.get(Permission.PermissionType.AUDIO).isGranted = true;
        }
    });

    private final ActivityResultLauncher<String> requestNotificationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
        if (isGranted) {
            permissionsMap.get(Permission.PermissionType.NOTIFICATION).isGranted = true;
        }
    });

    /**
     * Listener that ensures view is notified when any individual sensor(s) are toggled.
     */
    @Override
    public void onSensorToggle() {
        updateStartRecordingButtonState();
    }

}
