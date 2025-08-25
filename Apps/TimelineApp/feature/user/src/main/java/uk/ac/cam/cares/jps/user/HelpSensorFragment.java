package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.HelpSensorDocumentationBinding;

@AndroidEntryPoint
public class HelpSensorFragment extends Fragment {

    private HelpSensorDocumentationBinding binding;

    public HelpSensorFragment() {
        // Required empty public constructor
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpSensorDocumentationBinding.inflate(inflater, container, false);

        binding.topAppbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        setupExpandable(binding.contentAccelerometer, binding.iconExpandAccelerometer);
        setupExpandable(binding.contentActivity, binding.iconExpandActivity);
        setupExpandable(binding.contentGravity, binding.iconExpandGravity);
        setupExpandable(binding.contentGyroscope, binding.iconExpandGyroscope);
        setupExpandable(binding.contentLocation, binding.iconExpandLocation);
        setupExpandable(binding.contentMagnetometer, binding.iconExpandMagnetometer);
        setupExpandable(binding.contentLight, binding.iconExpandLight);
        setupExpandable(binding.contentHumidity, binding.iconExpandHumidity);
        setupExpandable(binding.contentPressure, binding.iconExpandPressure);

        return binding.getRoot();
    }

    private void setupExpandable(View content, ImageView icon) {
        View header = (View) icon.getParent();
        header.setOnClickListener(v -> {
            boolean isVisible = content.getVisibility() == View.VISIBLE;
            content.setVisibility(isVisible ? View.GONE : View.VISIBLE);
            icon.setRotation(isVisible ? 0 : 180);
        });
    }
}
