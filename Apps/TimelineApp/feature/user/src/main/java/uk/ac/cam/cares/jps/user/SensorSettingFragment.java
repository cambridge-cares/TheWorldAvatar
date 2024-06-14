package uk.ac.cam.cares.jps.user;

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

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentSensorSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.AccountViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;

@AndroidEntryPoint
public class SensorSettingFragment extends Fragment {

    private FragmentSensorSettingBinding binding;
    private SensorViewModel sensorViewModel;
    private AccountViewModel accountViewModel;
    private Map<Permission.PermissionType, Permission> permissionsMap = new HashMap<>();
    private List<Permission.PermissionType> criticalPermissionType = Arrays.asList(Permission.PermissionType.LOCATION_FINE, Permission.PermissionType.AUDIO);

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

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.topAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());

        binding.startRecordTv.setOnClickListener(this::onRecordButtonClicked);
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording) {
                binding.startRecordTv.setText(R.string.stop_recording);
            } else {
                binding.startRecordTv.setText(R.string.start_recording);
            }
        });
    }

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

        if (sensorViewModel.getIsRecording().getValue() != null && sensorViewModel.getIsRecording().getValue()) {
            sensorViewModel.stopRecording();
        } else {
            sensorViewModel.startRecording();
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
}
