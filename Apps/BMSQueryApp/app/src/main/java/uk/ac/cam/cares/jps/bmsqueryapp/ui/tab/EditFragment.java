package uk.ac.cam.cares.jps.bmsqueryapp.ui.tab;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.apache.log4j.BasicConfigurator;

import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentEditBinding;

public class EditFragment extends Fragment {
    private FragmentEditBinding binding;

    public EditFragment() {
        super();
    }

    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentEditBinding.inflate(inflater, container, false);
        BasicConfigurator.configure();
        return binding.getRoot();
    }
}
