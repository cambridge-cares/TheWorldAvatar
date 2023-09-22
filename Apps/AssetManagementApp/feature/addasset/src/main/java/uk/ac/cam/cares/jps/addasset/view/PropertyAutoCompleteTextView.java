package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;

import org.apache.log4j.Logger;

import java.util.List;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.DropDownDataModel;

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

    public PropertyAutoCompleteTextView(Context context, DropDownDataModel property) {
        super(context, R.layout.view_auto_complete_input_text_layout, property);
        this.context = context;

        editText = (AutoCompleteTextView) super.editText;
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item));

        editText.setOnItemClickListener((adapterView, view, i, l) -> {
            String selected = (String) adapterView.getItemAtPosition(i);
            property.setFieldValue(selected);
        });
        editText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i1, int i2) {;}

            @Override
            public void afterTextChanged(Editable editable) {
                property.getShowDisallowError().setValue(false);
                property.getIsMissingField().setValue(false);
                property.setFieldValue(editable.toString());
            }
        });
    }

    public void updateAdapterList(List<String> options) {
        // may be a bug with AutoCompleteTextView, the arrayAdapter.addAll() and notifyDatasetChanged() do not work
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, options));
    }
}
