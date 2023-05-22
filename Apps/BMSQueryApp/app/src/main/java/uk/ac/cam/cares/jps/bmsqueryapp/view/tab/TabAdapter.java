package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.data.attribtue.EditableAttribute;

public class TabAdapter extends FragmentStateAdapter {

    private final int NUM_TABS = 2;
    private VisualizationFragment dtvfTab;
    private List<EditableAttribute> editableAttributes;
    private String equipmentIri;
    private WebViewClient webViewClient;

    public TabAdapter(FragmentManager manager, Lifecycle lifecycle) {
        super(manager, lifecycle);
    }

    public void configDtvfTab(String equipmentIri, WebViewClient webViewClient) {
        this.equipmentIri = equipmentIri;
        this.webViewClient = webViewClient;
    }

    public void configEditTab(List<EditableAttribute> editableAttributes) {
        this.editableAttributes = editableAttributes;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        if (position == 0) {
            dtvfTab = new VisualizationFragment(equipmentIri, webViewClient);
            return dtvfTab;
        } else {
            return new EditFragment(editableAttributes);
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