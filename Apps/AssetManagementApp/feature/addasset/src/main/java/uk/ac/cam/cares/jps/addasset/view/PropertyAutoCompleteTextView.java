package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.widget.AdapterView;
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
    private TextWatcher defaultTextWatcher;

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

        defaultTextWatcher = new TextWatcher() {
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
        };
        editText.addTextChangedListener(defaultTextWatcher);
    }

    public <T> void updateAdapterList(List<T> options) {
        // may be a bug with AutoCompleteTextView, the arrayAdapter.addAll() and notifyDatasetChanged() do not work
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, options));
    }

    public <T> ArrayAdapter<T> getAdapter() {
        return (ArrayAdapter<T>) editText.getAdapter();
    }

    public void setOnItemClickedListener(AdapterView.OnItemClickListener onItemClickListener) {
        editText.setOnItemClickListener(onItemClickListener);
    }

    public void setTextWatcher(TextWatcher textWatcher) {
        editText.removeTextChangedListener(defaultTextWatcher);
        editText.addTextChangedListener(textWatcher);
    }
}
