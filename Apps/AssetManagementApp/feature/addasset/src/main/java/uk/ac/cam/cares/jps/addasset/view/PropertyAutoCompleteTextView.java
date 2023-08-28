package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.google.android.material.textfield.TextInputLayout;

import java.util.Arrays;
import java.util.List;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;

public class PropertyAutoCompleteTextView extends RelativeLayout {
    public PropertyAutoCompleteTextView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyAutoCompleteTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyAutoCompleteTextView(Context context, AssetPropertyDataModel property) {
        super(context);
        View view = inflate(getContext(), R.layout.view_auto_complete_input_text_layout, this);
        ((TextInputLayout) view.findViewById(R.id.input_layout)).setHint(property.getFieldName());

        AutoCompleteTextView textView = view.findViewById(R.id.auto_complete_tv);
        List<String> items = Arrays.asList("item1", "2item2", "item3");
        textView.setAdapter(new ArrayAdapter<>(context, R.layout.list_item, items));

        if (property.isDisallowInput()) {
            textView.setInputType(TextView.AUTO_SIZE_TEXT_TYPE_NONE);
        }
    }
}
