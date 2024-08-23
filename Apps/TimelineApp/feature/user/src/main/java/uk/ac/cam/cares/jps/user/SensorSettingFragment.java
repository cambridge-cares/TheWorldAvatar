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

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.source.handler.AccelerometerHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.GravitySensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.GyroscopeHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.LightSensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.LocationHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.MagnetometerHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.PressureSensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.RelativeHumiditySensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorManager;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.handler.SoundLevelHandler;
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
    @Inject
    SensorManager sensorManager;
    private SensorAdapter adapter;
    private boolean allToggledOn = false;


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

        List<SensorItem> sensorItems = new ArrayList<>();
        sensorItems.add(new SensorItem("Accelerometer", "Measures acceleration.", sensorManager.getSensorHandler(SensorType.ACCELEROMETER)));
        sensorItems.add(new SensorItem("Gyroscope", "Tracks rotation rate.", sensorManager.getSensorHandler(SensorType.GYROSCOPE)));
        sensorItems.add(new SensorItem("Magnetometer", "Detects magnetic fields.", sensorManager.getSensorHandler(SensorType.MAGNETOMETER)));
        sensorItems.add(new SensorItem("Light", "Senses light levels.", sensorManager.getSensorHandler(SensorType.LIGHT)));
        sensorItems.add(new SensorItem("Humidity", "Monitors air moisture.", sensorManager.getSensorHandler(SensorType.HUMIDITY)));
        sensorItems.add(new SensorItem("Pressure", "Gauges atmospheric pressure.", sensorManager.getSensorHandler(SensorType.PRESSURE)));
        sensorItems.add(new SensorItem("Gravity", "Detects gravity vector.", sensorManager.getSensorHandler(SensorType.GRAVITY)));
        sensorItems.add(new SensorItem("Location", "Tracks GPS position.", sensorManager.getSensorHandler(SensorType.LOCATION)));
        sensorItems.add(new SensorItem("Microphone", "Capture sound levels.", sensorManager.getSensorHandler(SensorType.SOUND)));

        adapter = new SensorAdapter(sensorItems, this);
        recyclerView.setAdapter(adapter);


        Button toggleAllBtn = view.findViewById(R.id.toggle_all_btn);
        toggleAllBtn.setOnClickListener(v -> {
            // Toggle the state
            allToggledOn = !allToggledOn;


            for (SensorItem item : adapter.getSensorItems()) {
                item.setToggled(allToggledOn); // Set the toggled state
            }

            // Notify the adapter to refresh the UI
            adapter.notifyDataSetChanged();


            toggleAllBtn.setText(allToggledOn ? "Toggle Off" : "Toggle All");

            // check if at least one sensor is toggled on to enable/disable Start Recording button
            updateStartRecordingButtonState();
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
        allToggledOn = false;
        binding.toggleAllBtn.setText("Toggle All");
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
        List<SensorType> selectedSensorTypes = new ArrayList<>();
        for (SensorItem item : adapter.getSensorItems()) {
            if (item.isToggled()) {
                selectedSensorTypes.add(item.getSensorHandler().getSensorType());
            }
        }


        if (sensorViewModel.getIsRecording().getValue() != null && sensorViewModel.getIsRecording().getValue()) {
            // Stop recording and reset sensors
            sensorViewModel.stopRecording();
            for (SensorItem item : adapter.getSensorItems()) {
                item.getSensorHandler().stop();
                item.setToggled(false); // Reset toggles
            }
            adapter.setTogglesEnabled(true);
            binding.toggleAllBtn.setEnabled(true);
            updateStartRecordingButtonState();
        } else {

            // Start recording only the selected sensors
            sensorViewModel.startRecording(selectedSensorTypes);
            adapter.setTogglesEnabled(false);
            binding.toggleAllBtn.setEnabled(false); // Disable "Toggle All" during recording

        }
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
