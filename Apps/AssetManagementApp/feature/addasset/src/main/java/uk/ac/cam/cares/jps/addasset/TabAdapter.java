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

import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;

public class TabAdapter extends FragmentStateAdapter {
    private final int NUM_TABS = 3;
    private AddAssetViewModel viewModel;

    public TabAdapter(@NonNull FragmentManager fragmentManager, @NonNull Lifecycle lifecycle, AddAssetViewModel viewModel) {
        super(fragmentManager, lifecycle);
        this.viewModel = viewModel;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        if (position == 0) {
            return new TabFragment(Arrays.asList(BASIC_SECTION_TITLE, LOCATION_SECTION_TITLE, SUPPLIER_SECTION_TITLE), viewModel);
        } else if (position == 1) {
            return new TabFragment(Arrays.asList(PURCHASE_SECTION_TITLE, ITEM_SECTION_TITLE), viewModel);
        } else {
            return new TabFragment(Arrays.asList(SPEC_SHEET_SECTION_TITLE, MANUAL_SECTION_TITLE), viewModel);
        }
    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }
}
