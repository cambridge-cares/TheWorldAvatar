package uk.ac.cam.cares.jps.bmsqueryapp.ui.tab;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

/**
 * A [FragmentPagerAdapter] that returns a fragment corresponding to
 * one of the sections/tabs/pages.
 */
public class TabAdapter extends FragmentStateAdapter {

    private final int NUM_TABS = 2;
    private VisualizationFragment dtvfTab;

    public TabAdapter(FragmentManager manager, Lifecycle lifecycle) {
        super(manager, lifecycle);
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        if (position == 0) {
            dtvfTab = new VisualizationFragment();
            return dtvfTab;
        } else {
            return new EditFragment();
        }

    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }

    public VisualizationFragment getDtvfTab() {
        return dtvfTab;
    }
}