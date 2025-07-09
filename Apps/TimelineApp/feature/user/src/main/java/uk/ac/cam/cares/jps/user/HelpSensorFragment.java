package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.R;
import uk.ac.cam.cares.jps.user.databinding.HelpSensorDocumentationBinding;

@AndroidEntryPoint
public class HelpSensorFragment extends Fragment {

    private HelpSensorDocumentationBinding binding;

    public HelpSensorFragment() {
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpSensorDocumentationBinding.inflate(inflater, container, false);

        binding.topAppbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        return binding.getRoot();
    }
}
