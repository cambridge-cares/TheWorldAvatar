package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;

import uk.ac.cam.cares.jps.addasset.R;

public class PropertyAutoCompleteTextView extends RelativeLayout {
    public PropertyAutoCompleteTextView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_input_text_layout, this);
    }

    public PropertyAutoCompleteTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }
}
