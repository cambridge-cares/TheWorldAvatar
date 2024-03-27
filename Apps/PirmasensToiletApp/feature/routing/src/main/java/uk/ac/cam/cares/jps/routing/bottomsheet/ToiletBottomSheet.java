package uk.ac.cam.cares.jps.routing.bottomsheet;

import android.graphics.Bitmap;
import android.os.Handler;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LifecycleRegistry;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.squareup.picasso.MemoryPolicy;
import com.squareup.picasso.NetworkPolicy;
import com.squareup.picasso.Picasso;
import com.squareup.picasso.PicassoProvider;

import org.apache.log4j.Logger;

import java.util.Map;

import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.routing.R;
import uk.ac.cam.cares.jps.routing.RoutingFragment;
import uk.ac.cam.cares.jps.routing.ui.manager.ToiletMarkerManager;
import uk.ac.cam.cares.jps.routing.viewmodel.LocationViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.RoutingViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.ToiletViewModel;


public class ToiletBottomSheet {
    private Logger LOGGER = Logger.getLogger(ToiletBottomSheet.class);
    private final ToiletViewModel toiletViewModel;
    private final RoutingViewModel routingViewModel;
    private final LocationViewModel locationViewModel;

    private final LinearLayoutCompat bottomSheetView;
    public BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private Picasso picasso;

    public ToiletBottomSheet(Fragment hostFragment, LinearLayoutCompat bottomSheetView) {
        locationViewModel = new ViewModelProvider(hostFragment).get(LocationViewModel.class);

        routingViewModel = new ViewModelProvider(hostFragment).get(RoutingViewModel.class);

        toiletViewModel = new ViewModelProvider(hostFragment).get(ToiletViewModel.class);

        this.bottomSheetBehavior = BottomSheetBehavior.from(bottomSheetView);
        this.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_HIDDEN);
        this.bottomSheetView = bottomSheetView;
        Picasso.Builder builder = new Picasso.Builder(hostFragment.getContext());
        builder.defaultBitmapConfig(Bitmap.Config.RGB_565);
        picasso = builder.build();
        picasso.setLoggingEnabled(true);
        Picasso.setSingletonInstance(picasso);
        init(hostFragment);
    }

    private void init(Fragment hostFragment) {

        toiletViewModel.getSelectedToilet().observe(hostFragment, toilet -> {
            // ((TextView) bottomSheetView.findViewById(R.id.address_name_tv)).setText(!toilet.getName().isEmpty() ? toilet.getName() : toilet.getAddress());
            ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_happy))
                    .setImageResource(R.drawable.emoji_smile_black_24);
            ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_neutral))
                    .setImageResource(R.drawable.emoji_neutral_balck_24);
            ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_sad))
                    .setImageResource(R.drawable.emoji_sad_black_24);
            if(toilet.getAverageRating().equals("3")){
                ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_happy))
                        .setImageResource(R.drawable.emoji_smile_color_24);
            }else if(toilet.getAverageRating().equals("2")){
                ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_neutral))
                        .setImageResource(R.drawable.emoji_neutral_color_24);
            }else if(toilet.getAverageRating().equals("1")){
                ((ImageView) bottomSheetView.findViewById(R.id.feeling_icon_sad))
                        .setImageResource(R.drawable.emoji_sad_color_24);
            }

            ((ImageView) bottomSheetView.findViewById(R.id.has_female_icon))
                    .setColorFilter(toilet.getHasFemale() ? ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.female_toilet) : ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.grey));
            ((ImageView) bottomSheetView.findViewById(R.id.has_males_icon))
                    .setColorFilter(toilet.getHasMale() ? ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.male_toilet) : ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.grey));
            ((ImageView) bottomSheetView.findViewById(R.id.wheelchair_icon))
                    .setColorFilter(!toilet.getWheelchair().isEmpty() ? ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.teal_700) : ContextCompat.getColor(hostFragment.requireContext(), uk.ac.cam.cares.jps.ui.R.color.grey));

            if (toilet.getAddress() != null && !toilet.getAddress().isEmpty()) {
                bottomSheetView.findViewById(R.id.address_container).setVisibility(View.VISIBLE);
                ((TextView) bottomSheetView.findViewById(R.id.detailed_address_tv)).setText(toilet.getAddress());
            } else {
                bottomSheetView.findViewById(R.id.address_container).setVisibility(View.GONE);
            }

            if (toilet.getPrice() != null) {
                bottomSheetView.findViewById(R.id.price_container).setVisibility(View.VISIBLE);
                ((TextView) bottomSheetView.findViewById(R.id.price)).setText(toilet.getPrice().toString());
            } else {
                bottomSheetView.findViewById(R.id.price_container).setVisibility(View.GONE);
            }

            if (toilet.getImage() != null && toilet.getImage().isEmpty() == false) {
                bottomSheetView.findViewById(R.id.cover_image).setVisibility(View.VISIBLE);
                LOGGER.debug("LOAD IMAGE TOILET " + toilet.getImage().replace("localhost", "192.168.0.12"));
                Picasso.get()
                        .load(toilet.getImage().replace("localhost", NetworkConfiguration.HOST))
                        .into(((ImageView) bottomSheetView.findViewById(R.id.cover_image)));
            } else {
                bottomSheetView.findViewById(R.id.cover_image).setVisibility(View.GONE);
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

            // todo: route retrieved when click the marker, show route when click on the button
            bottomSheetView.findViewById(R.id.direction_bt).setOnClickListener(view1 -> {
                if (locationViewModel.getCurrentLocationValue() == null) {
                    LOGGER.info("Location permission not granted, launch permission intent");
                    ((RoutingFragment) hostFragment).showLocationPermissionDeniedDialog();
                    return;
                }

                routingViewModel.showRoute.postValue(true);
            });
        });

        routingViewModel.routeGeoJsonData.observe(hostFragment, route -> {
            ((TextView) bottomSheetView.findViewById(R.id.distance_tv)).setText(route.getCostWithUnit());
            ((TextView) bottomSheetView.findViewById(R.id.route_time_estimation_tv)).setText(route.getWalkingTime());
        });
    }

}
