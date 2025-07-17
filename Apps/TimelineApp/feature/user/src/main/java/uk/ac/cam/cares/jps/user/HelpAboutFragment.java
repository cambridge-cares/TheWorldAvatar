package uk.ac.cam.cares.jps.user;

import android.content.SharedPreferences;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.HelpAboutBinding;
import uk.ac.cam.cares.jps.ui.viewmodel.TooltipTriggerViewModel;
import uk.ac.cam.cares.jps.user.R;

@AndroidEntryPoint
public class HelpAboutFragment extends Fragment {

    private HelpAboutBinding binding;
    private TooltipTriggerViewModel tooltipTriggerViewModel;

    public HelpAboutFragment() {}

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpAboutBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        tooltipTriggerViewModel = new ViewModelProvider(requireActivity()).get(TooltipTriggerViewModel.class);

        binding.section1Row.setOnClickListener(v -> toggleSectionVisibility(binding.section1Subtitle));
        binding.section2Row.setOnClickListener(v -> toggleSectionVisibility(binding.section2Subtitle));
        binding.section3Row.setOnClickListener(v -> toggleSectionVisibility(binding.section3Subtitle));

        binding.topAppbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        binding.btnTriggerTooltips.setOnClickListener(v -> {
            SharedPreferences prefs = requireContext().getSharedPreferences("tooltip_prefs", Context.MODE_PRIVATE);
            prefs.edit().putBoolean("tooltip_skip", false).apply();

            tooltipTriggerViewModel.requestTooltipTrigger();
            Toast.makeText(requireContext(), "Tooltips will show on the main screen.", Toast.LENGTH_SHORT).show();
            NavHostFragment.findNavController(this).navigateUp();
            NavHostFragment.findNavController(this).navigateUp();
        });

    }

    private void toggleSectionVisibility(TextView subtitle) {
        subtitle.setVisibility(subtitle.getVisibility() == View.VISIBLE ? View.GONE : View.VISIBLE);
    }
}
