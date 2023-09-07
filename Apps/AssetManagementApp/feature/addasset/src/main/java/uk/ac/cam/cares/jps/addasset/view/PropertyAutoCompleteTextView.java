package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.ListAdapter;
import android.widget.TextView;

import androidx.lifecycle.ViewModelProvider;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AddAssetViewModel;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;
import uk.ac.cam.cares.jps.data.OtherInfoModel;

public class PropertyAutoCompleteTextView extends PropertyBaseInputTextView {

    AutoCompleteTextView editText;
    Context context;
    private Logger LOGGER = Logger.getLogger(PropertyAutoCompleteTextView.class);

    public PropertyAutoCompleteTextView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_auto_complete_input_text_layout, this);
    }

    public PropertyAutoCompleteTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_auto_complete_input_text_layout, this);
    }

    public PropertyAutoCompleteTextView(Context context, AssetPropertyDataModel property) {
        super(context, R.layout.view_auto_complete_input_text_layout, property);
        this.context = context;

        editText = (AutoCompleteTextView) super.editText;
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item));

        if (property.isDisallowInput()) {
            editText.setInputType(TextView.AUTO_SIZE_TEXT_TYPE_NONE);
        }
    }

    public void updateAdapterList(List<OtherInfoModel> options) {
        // may be a bug with AutoCompleteTextView, the arrayAdapter.addAll() and notifyDatasetChanged() do not work
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, options));
    }
}
