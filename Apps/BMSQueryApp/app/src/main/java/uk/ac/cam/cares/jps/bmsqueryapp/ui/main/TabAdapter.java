package uk.ac.cam.cares.jps.bmsqueryapp.ui.main;

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

    private final int NUM_TABS = 4;

    public TabAdapter(FragmentManager manager, Lifecycle lifecycle) {
        super(manager, lifecycle);
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        return new TabFragment(position);
    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }
}