package uk.ac.cam.cares.jps.routing.bottomsheet;

import android.content.Context;
import android.os.Bundle;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.coordinatorlayout.widget.CoordinatorLayout;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;

import java.util.Map;

import uk.ac.cam.cares.jps.routing.R;
import uk.ac.cam.cares.jps.routing.viewmodel.LocationViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.RoutingViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.ToiletViewModel;
import uk.ac.cam.cares.jps.routing.databinding.ToiletBottomSheetBinding;

public class ToiletBottomSheet {

    private final ToiletViewModel toiletViewModel;
    private final RoutingViewModel routingViewModel;
    private final LocationViewModel locationViewModel;

    private final LinearLayoutCompat bottomSheetView;
    public BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;

    public ToiletBottomSheet(Fragment hostFragment, LinearLayoutCompat bottomSheetView) {
        locationViewModel = new ViewModelProvider(hostFragment).get(LocationViewModel.class);

        routingViewModel = new ViewModelProvider(hostFragment).get(RoutingViewModel.class);

        toiletViewModel = new ViewModelProvider(hostFragment).get(ToiletViewModel.class);

        this.bottomSheetBehavior = BottomSheetBehavior.from(bottomSheetView);
        this.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_HIDDEN);
        this.bottomSheetView = bottomSheetView;

        init(hostFragment);
    }

    private void init(Fragment hostFragment) {
        toiletViewModel.getSelectedToilet().observe(hostFragment.getViewLifecycleOwner(), toilet -> {
            ((TextView) bottomSheetView.findViewById(R.id.address_name_tv)).setText(!toilet.getName().isEmpty() ? toilet.getName() : String.format("(%f, %f)", toilet.getLocation().longitude(), toilet.getLocation().latitude()));

            if (toilet.getHasFemale()) {
                ((ImageView) bottomSheetView.findViewById(R.id.has_female_icon)).setColorFilter(uk.ac.cam.cares.jps.ui.R.color.black);
            }
            if (toilet.getHasMale()) {
                ((ImageView) bottomSheetView.findViewById(R.id.has_males_icon)).setColorFilter(uk.ac.cam.cares.jps.ui.R.color.black);
            }
            if (!toilet.getWheelchair().isEmpty()) {
                ((ImageView) bottomSheetView.findViewById(R.id.wheelchair_icon)).setColorFilter(uk.ac.cam.cares.jps.ui.R.color.black);
            }

            if (!toilet.getOpenTime().isEmpty() || !toilet.getEndTime().isEmpty()) {
                ((TextView) bottomSheetView.findViewById(R.id.open_hour_tv)).setText(String.format("%s - %s", toilet.getOpenTime(), toilet.getEndTime()));
            } else {
                bottomSheetView.findViewById(R.id.open_hour_container).setVisibility(View.GONE);
            }

            if (toilet.getPrice() != null) {
                ((TextView) bottomSheetView.findViewById(R.id.price)).setText(toilet.getPrice().toString());
            } else if (!toilet.getFee().isEmpty()) {
                ((TextView) bottomSheetView.findViewById(R.id.price)).setText(toilet.getFee());
            } else {
                bottomSheetView.findViewById(R.id.price_container).setVisibility(View.GONE);
            }


            ((LinearLayout) bottomSheetView.findViewById(R.id.other_info_container)).removeAllViews();

            // other info
            for (Map.Entry<String, String> otherInfo : toilet.getOtherInfo().entrySet()) {
                if (otherInfo.getValue().isEmpty()) {
                    continue;
                }

                LinearLayout itemContainer = new LinearLayout(hostFragment.requireContext());
                itemContainer.setOrientation(LinearLayout.HORIZONTAL);

                TextView title = new TextView(hostFragment.requireContext());
                title.setText(otherInfo.getKey());
                title.setTextAppearance(com.google.android.material.R.style.TextAppearance_Material3_TitleMedium);

                itemContainer.addView(title);

                TextView content = new TextView(hostFragment.requireContext());
                content.setText(otherInfo.getValue());
                content.setTextAppearance(com.google.android.material.R.style.TextAppearance_Material3_BodyMedium);
                content.setLayoutParams(new ViewGroup.MarginLayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,
                        ViewGroup.LayoutParams.WRAP_CONTENT));
                ViewGroup.MarginLayoutParams marginLayoutParams = (ViewGroup.MarginLayoutParams) content.getLayoutParams();
                marginLayoutParams.setMarginStart(20);
                itemContainer.addView(content);

                ((LinearLayout) bottomSheetView.findViewById(R.id.other_info_container)).addView(itemContainer);
            }

            ((Button) bottomSheetView.findViewById(R.id.direction_bt)).setOnClickListener(view1 -> routingViewModel.getRouteData(locationViewModel.getCurrentLocationValue().longitude(),
                    locationViewModel.getCurrentLocationValue().latitude(),
                    toilet.getLocation().longitude(),
                    toilet.getLocation().latitude()));
        });
    }
}
