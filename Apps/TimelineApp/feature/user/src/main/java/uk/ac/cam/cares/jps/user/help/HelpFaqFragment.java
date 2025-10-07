package uk.ac.cam.cares.jps.user.help;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.appbar.MaterialToolbar;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.HelpFaqBinding;

@AndroidEntryPoint
public class HelpFaqFragment extends Fragment {

    private HelpFaqBinding binding;

    public HelpFaqFragment() {
        // Required empty public constructor
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpFaqBinding.inflate(inflater, container, false);

        MaterialToolbar toolbar = binding.topAppbar;
        toolbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        setupToggle(binding.faq1Title, binding.faq1Answer);
        setupToggle(binding.faq2Title, binding.faq2Answer);
        setupToggle(binding.faq3Title, binding.faq3Answer);
        setupToggle(binding.faq4Title, binding.faq4Answer);
        setupToggle(binding.faq5Title, binding.faq5Answer);
        setupToggle(binding.faq6Title, binding.faq6Answer);
        setupToggle(binding.faq7Title, binding.faq7Answer);
        setupToggle(binding.faq8Title, binding.faq8Answer);

        return binding.getRoot();
    }

    private void setupToggle(TextView titleView, TextView answerView) {
        titleView.setOnClickListener(v -> {
            if (answerView.getVisibility() == View.VISIBLE) {
                answerView.setVisibility(View.GONE);
            } else {
                answerView.setVisibility(View.VISIBLE);
            }
        });
    }
}
