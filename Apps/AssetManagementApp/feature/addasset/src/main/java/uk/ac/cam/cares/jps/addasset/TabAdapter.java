package uk.ac.cam.cares.jps.addasset;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BASIC_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.ITEM_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.LOCATION_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.PURCHASE_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SUPPLIER_SECTION_TITLE;

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
            return new TabFragment(Arrays.asList(BASIC_SECTION_TITLE, LOCATION_SECTION_TITLE, SUPPLIER_SECTION_TITLE));
        } else if (position == 1) {
            return new TabFragment(Arrays.asList(PURCHASE_SECTION_TITLE, ITEM_SECTION_TITLE));
        } else {
            // todo: maybe need a separate fragment for datasheet, because the section items look very different from other input fields
            return new TabFragment(Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE));
        }
    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }
}
