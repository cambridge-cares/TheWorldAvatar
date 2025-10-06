package uk.ac.cam.cares.jps.sensor.permission;


import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.Fragment;

import org.apache.log4j.Logger;

import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class PermissionHelper {
    private final Fragment fragment;
    private Queue<String> permissionQueue = new LinkedList<>();
    private ActivityResultLauncher<String> launcher;
    private final Logger LOGGER;
    private Runnable onAllPermissionsHandled;

    public PermissionHelper(Fragment fragment) {
        this.fragment = fragment;
        LOGGER = Logger.getLogger(PermissionHelper.class.getName() + "-" + fragment.getClass().getName());
        registerPermissionLauncher();
    }

    private void registerPermissionLauncher() {
        if (launcher == null) {
            launcher = fragment.registerForActivityResult(
                    new ActivityResultContracts.RequestPermission(),
                    isGranted -> handlePermissionResult(isGranted)
            );
        }
    }

    public void requestPermissionsInChain(List<String> permissions, Runnable onAllPermissionsHandled) {
        LOGGER.info(String.format("Received permissions: %s", permissions));
        this.onAllPermissionsHandled = onAllPermissionsHandled;

        permissionQueue.clear();
        permissionQueue.addAll(permissions);
        launchNextPermission();
    }

    private void launchNextPermission() {
        String nextPermission = permissionQueue.poll();
        if (nextPermission != null) {
            launcher.launch(nextPermission);
        } else {
            LOGGER.info("All permissions processed!");
            onAllPermissionsHandled.run();
        }
    }

    private void handlePermissionResult(boolean granted) {
        if (granted) {
            launchNextPermission();
        }
    }

}
