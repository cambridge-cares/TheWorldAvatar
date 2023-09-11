package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.TextView;

import org.apache.log4j.Logger;

import java.util.List;

import uk.ac.cam.cares.jps.addasset.R;
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
        // todo: what if user typed the full name, but not click item?
        // todo: another way will be when collecting the field's value, check in **VM** whether the option dict has this name. But name is not the identifier of the object, so not 100% gauranteed
        editText.setOnItemClickListener((adapterView, view, i, l) -> {
            OtherInfoModel selected = (OtherInfoModel) adapterView.getItemAtPosition(i);
            property.setValueIri(selected.getIri());
            property.setFieldValue(selected.getName());
        });

        if (property.isDisallowInput()) {
            editText.setInputType(TextView.AUTO_SIZE_TEXT_TYPE_NONE);
        }
    }

    public void updateAdapterList(List<OtherInfoModel> options) {
        // may be a bug with AutoCompleteTextView, the arrayAdapter.addAll() and notifyDatasetChanged() do not work
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, options));
    }
}
