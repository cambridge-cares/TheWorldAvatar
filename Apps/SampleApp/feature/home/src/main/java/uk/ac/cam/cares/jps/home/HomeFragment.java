package uk.ac.cam.cares.jps.home;

import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import uk.ac.cam.cares.jps.home.databinding.HomeFragmentBinding;

public class HomeFragment extends Fragment {
    private HomeFragmentBinding binding;

    // Inflate the XML layout for this fragment into a collection of View objects that can be manipulated in code.
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = HomeFragmentBinding.inflate(inflater);
        return binding.getRoot();
    }

    // Actions to be taken after view is created
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.photoBt.setOnClickListener(view1 -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.photo_fragment_link)))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });
        binding.todoBt.setOnClickListener(view1 -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.todo_fragment_link)))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });
    }
}
