package uk.ac.cam.cares.jps.timeline;

import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.PopupWindow;
import android.widget.TextView;
import android.graphics.drawable.ColorDrawable;
import android.graphics.Color;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.navigation.NavController;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.timeline.databinding.ActivityMainBinding;
import uk.ac.cam.cares.jps.ui.UiUtils;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {
    private static ActivityMainBinding binding;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        EdgeToEdge.enable(this);
        super.onCreate(savedInstanceState);

        binding = ActivityMainBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());


        binding.userIcon.setOnClickListener(v -> showUserDropdown(v));

        // Check if onboarding was already seen
        SharedPreferences prefs = getSharedPreferences("onboarding", MODE_PRIVATE);
        boolean hasSeenOnboarding = prefs.getBoolean("seen", false);

        if (hasSeenOnboarding) {
            new Handler(Looper.getMainLooper()).post(() -> {
                NavHostFragment navHostFragment =
                        (NavHostFragment) getSupportFragmentManager().findFragmentById(R.id.nav_host_fragment);
                if (navHostFragment != null) {
                    NavController navController = navHostFragment.getNavController();
                    Uri uri = Uri.parse(getString(R.string.login_fragment_link));
                    NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                            .fromUri(uri)
                            .build();
                    navController.navigate(request);
                }
            });
        }
    }

    private void showUserDropdown(View anchor) {
        View popupView = LayoutInflater.from(this).inflate(R.layout.view_user_dropdown_menu, null);
        PopupWindow popupWindow = new PopupWindow(
                popupView,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true
        );

        popupWindow.setElevation(10f);
        popupWindow.setOutsideTouchable(true);
        popupWindow.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));

        // Link all dropdown menu items
        popupView.findViewById(R.id.menu_account).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo(R.id.action_global_account_setting);
        });

        popupView.findViewById(R.id.menu_sensor).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo(R.id.action_global_sensor_setting);
        });

        popupView.findViewById(R.id.menu_timeline).setOnClickListener(v -> {
            popupWindow.dismiss();
            showNotImplemented();
        });

        popupView.findViewById(R.id.menu_help).setOnClickListener(v -> {
            popupWindow.dismiss();
            showNotImplemented();
        });

        popupView.findViewById(R.id.menu_privacy).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo(R.id.action_global_privacy_setting);
        });

        popupView.findViewById(R.id.menu_logout).setOnClickListener(v -> {
            popupWindow.dismiss();
            // TODO: Add logout logic here
            showNotImplemented();
        });

        popupWindow.showAsDropDown(anchor, -100, 20);
    }

    private void navigateTo(int actionId) {
        NavHostFragment navHostFragment =
                (NavHostFragment) getSupportFragmentManager().findFragmentById(R.id.nav_host_fragment);
        if (navHostFragment != null) {
            navHostFragment.getNavController().navigate(actionId);
        }
    }

    private void showNotImplemented() {
        UiUtils.showNotImplementedDialog(this); // replace with a Toast or dialog if needed
    }
}