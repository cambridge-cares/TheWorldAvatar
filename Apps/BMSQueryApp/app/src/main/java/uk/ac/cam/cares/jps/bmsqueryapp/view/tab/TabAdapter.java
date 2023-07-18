package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.webkit.ValueCallback;
import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.List;

import uk.ac.cam.cares.jps.bmsqueryapp.data.attribute.EditableAttribute;

public class TabAdapter extends FragmentStateAdapter {

    private final int NUM_TABS = 2;
    private VisualizationFragment dtvfTab;
    private EditFragment editTab;
    private List<EditableAttribute> editableAttributes;
    private String equipmentIri;
    private WebViewClient webViewClient;
    private ValueCallback<String> reloadCallback;

    public TabAdapter(FragmentManager manager, Lifecycle lifecycle) {
        super(manager, lifecycle);
    }

    public void configDtvfTab(String equipmentIri, WebViewClient webViewClient, ValueCallback<String> reloadCallback) {
        this.equipmentIri = equipmentIri;
        this.webViewClient = webViewClient;
        this.reloadCallback = reloadCallback;
    }

    public void configEditTab(List<EditableAttribute> editableAttributes) {
        this.editableAttributes = editableAttributes;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        if (position == 0) {
            dtvfTab = new VisualizationFragment(equipmentIri, webViewClient, reloadCallback);
            return dtvfTab;
        } else {
            editTab = new EditFragment(editableAttributes);
            return editTab;
        }
    }

    @Override
    public int getItemCount() {
        return NUM_TABS;
    }

    public VisualizationFragment getDtvfTab() {
        return dtvfTab;
    }

    public EditFragment getEditTab() {
        return editTab;
    }
}