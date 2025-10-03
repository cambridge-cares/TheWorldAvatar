package uk.ac.cam.cares.jps.user;

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

import uk.ac.cam.cares.jps.user.R;

public class HelpMapFragment extends Fragment {

    public HelpMapFragment() {
        // Required empty constructor
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {

        View view = inflater.inflate(R.layout.help_map_interactions, container, false);

        MaterialToolbar toolbar = view.findViewById(R.id.top_appbar);
        toolbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        TextView titleMapOverview = view.findViewById(R.id.title_map_overview);
        View contentMapOverview = view.findViewById(R.id.content_map_overview);
        titleMapOverview.setOnClickListener(v -> {
            toggleVisibility(contentMapOverview);
        });

        TextView titleMapUsage = view.findViewById(R.id.title_map_usage);
        View contentMapUsage = view.findViewById(R.id.content_map_usage);
        titleMapUsage.setOnClickListener(v -> {
            toggleVisibility(contentMapUsage);
        });

        TextView titleIconLegend = view.findViewById(R.id.title_icon_legend);
        View contentIconLegend = view.findViewById(R.id.content_icon_legend);
        titleIconLegend.setOnClickListener(v -> {
            toggleVisibility(contentIconLegend);
        });

        return view;
    }

    private void toggleVisibility(View content) {
        if (content.getVisibility() == View.VISIBLE) {
            content.setVisibility(View.GONE);
        } else {
            content.setVisibility(View.VISIBLE);
        }
    }
}
