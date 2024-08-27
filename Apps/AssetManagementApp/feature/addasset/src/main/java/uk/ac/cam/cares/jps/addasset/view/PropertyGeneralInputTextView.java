package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.text.InputType;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.AssetPropertyDataModel;

public class PropertyGeneralInputTextView extends PropertyBaseInputTextView {
    private final Logger LOGGER = Logger.getLogger(PropertyGeneralInputTextView.class);

    public PropertyGeneralInputTextView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyGeneralInputTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyGeneralInputTextView(Context context, AssetPropertyDataModel property) {
        super(context, R.layout.view_input_text_layout, property);
        if (property.isMultiLine()) {
            editText.setMinLines(6);
            editText.setGravity(Gravity.START | Gravity.TOP);
        }
    }


}
