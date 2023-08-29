package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BASIC;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.ITEM;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATION;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.PURCHASE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SUPPLIER;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.Arrays;

public class TabAdapter extends FragmentStateAdapter {
    private final int NUM_TABS = 3;

    public TabAdapter(@NonNull FragmentManager fragmentManager, @NonNull Lifecycle lifecycle) {
        super(fragmentManager, lifecycle);
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        if (position == 0) {
            return new TabFragment(Arrays.asList(BASIC, LOCATION, SUPPLIER));
        } else if (position == 1) {
            return new TabFragment(Arrays.asList(PURCHASE, ITEM));
        } else {
            // todo: maybe need a separate fragment for datasheet, because the section items look very different from other input fields
            return new TabFragment(Arrays.asList(SPEC_SHEET, MANUAL));
        }
    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }
}
