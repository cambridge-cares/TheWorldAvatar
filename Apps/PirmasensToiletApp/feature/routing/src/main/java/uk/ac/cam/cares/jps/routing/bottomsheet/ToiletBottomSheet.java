package uk.ac.cam.cares.jps.routing.bottomsheet;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import java.util.Map;

import uk.ac.cam.cares.jps.routing.viewmodel.LocationViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.RoutingViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.ToiletViewModel;
import uk.ac.cam.cares.jps.routing.databinding.ToiletBottomSheetBinding;

public class ToiletBottomSheet extends BottomSheetDialogFragment {

    private ToiletBottomSheetBinding binding;
    private ToiletViewModel toiletViewModel;
    private RoutingViewModel routingViewModel;
    private LocationViewModel locationViewModel;

    public ToiletBottomSheet(Fragment hostFragment) {
        locationViewModel = new ViewModelProvider(hostFragment).get(LocationViewModel.class);

        routingViewModel = new ViewModelProvider(hostFragment).get(RoutingViewModel.class);

        toiletViewModel = new ViewModelProvider(hostFragment).get(ToiletViewModel.class);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = ToiletBottomSheetBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        toiletViewModel.getSelectedToilet().observe(this.getViewLifecycleOwner(), toilet -> {
            binding.addressNameTv.setText(toilet.getName());

            // other info
            for (Map.Entry<String, String> otherInfo : toilet.getOtherInfo().entrySet()) {
                LinearLayout itemContainer = new LinearLayout(requireContext());
                itemContainer.setOrientation(LinearLayout.HORIZONTAL);

                TextView title = new TextView(requireContext());
                title.setText(otherInfo.getKey());
                TextView content = new TextView(requireContext());
                content.setText(otherInfo.getValue());

                itemContainer.addView(title);
                itemContainer.addView(content);

                binding.otherInfoContainer.addView(itemContainer);
            }

            binding.directionBt.setOnClickListener(view1 -> routingViewModel.getRouteData(locationViewModel.getCurrentLocationValue().longitude(),
                    locationViewModel.getCurrentLocationValue().latitude(),
                    toilet.getLocation().longitude(),
                    toilet.getLocation().latitude()));
        });


    }
}
