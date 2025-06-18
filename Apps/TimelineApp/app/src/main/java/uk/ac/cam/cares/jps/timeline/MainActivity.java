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

        // Check if onboarding was already seen
        SharedPreferences prefs = getSharedPreferences("onboarding", MODE_PRIVATE);
        boolean hasSeenOnboarding = prefs.getBoolean("seen", false);

        if (hasSeenOnboarding) {
            new Handler(Looper.getMainLooper()).post(() -> {
                NavHostFragment navHostFragment =
                        (NavHostFragment) getSupportFragmentManager().findFragmentById(R.id.nav_host_fragment);
                if (navHostFragment != null) {
                    NavController navController = navHostFragment.getNavController();
                    Uri uri = Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.onboarding_fragment_link));
                    NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                            .fromUri(uri)
                            .build();
                    navController.navigate(request);
                }
            });
        }
    }


}