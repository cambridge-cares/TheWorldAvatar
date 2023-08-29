package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.TextView;

import java.util.Arrays;
import java.util.List;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;

public class PropertyAutoCompleteTextView extends PropertyBaseInputTextView {

    AutoCompleteTextView editText;

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
        editText = (AutoCompleteTextView) super.editText;

        List<String> items = Arrays.asList("item1", "2item2", "item3");
        editText.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, items));

        if (property.isDisallowInput()) {
            editText.setInputType(TextView.AUTO_SIZE_TEXT_TYPE_NONE);
        }
    }
}
