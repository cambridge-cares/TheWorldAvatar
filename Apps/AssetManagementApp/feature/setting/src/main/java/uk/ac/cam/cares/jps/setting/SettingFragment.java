package uk.ac.cam.cares.jps.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.setting.databinding.FragmentSettingBinding;

public class SettingFragment extends Fragment {
    private FragmentSettingBinding binding;
    private final Logger LOGGER = Logger.getLogger(SettingFragment.class);
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentSettingBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        binding.backBt.setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
    }
}
