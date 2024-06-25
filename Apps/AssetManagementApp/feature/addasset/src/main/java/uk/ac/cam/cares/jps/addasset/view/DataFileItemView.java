package uk.ac.cam.cares.jps.addasset.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.addasset.R;
import uk.ac.cam.cares.jps.addasset.model.DataFileDataModel;

public class DataFileItemView extends RelativeLayout {
    private final Logger LOGGER = Logger.getLogger(DataFileItemView.class);
    private Context context;
    private View view;


    public DataFileItemView(Context context) {
        super(context);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public DataFileItemView(Context context, AttributeSet attrs) {
        super(context, attrs);
        inflate(getContext(), R.layout.view_data_sheet_item, this);
    }



    public DataFileItemView(Context context, DataFileDataModel property) {
        super(context);

//        this.activityResultCaller = activityResultCaller;
        this.context = context;

        view = inflate(getContext(), R.layout.view_data_sheet_item, this);
    }

    public void setOnButtonClickListener(OnClickListener onClickListener) {
        view.findViewById(R.id.upload_bt).setOnClickListener(onClickListener);
    }

    public void setFileNameTextView(String fileName) {
        ((TextView) view.findViewById(R.id.file_name_tv)).setText(fileName + " is selected.");
        view.findViewById(R.id.file_name_tv).setVisibility(VISIBLE);
    }
}
