package uk.ac.cam.cares.jps.timeline.ui.datepicker;

import android.content.Context;
import android.content.res.ColorStateList;
import android.os.Parcel;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.color.MaterialColors;
import com.google.android.material.datepicker.DayViewDecorator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;

public class GreyOutDecorator extends DayViewDecorator {
    private ColorStateList greyColor;
    private ColorStateList normalColor;
    private Map<YearMonthCompositeKey, List<Integer>> datesWithTrajectory = new HashMap<>();  // in local timezone

    @Override
    public void initialize(@NonNull Context context) {
        super.initialize(context);
        int textColor = MaterialColors.getColor(context, com.google.android.material.R.attr.colorOutline, GreyOutDecorator.class.getSimpleName());
        greyColor = ColorStateList.valueOf(textColor);

        int normalTextColor = MaterialColors.getColor(context, com.google.android.material.R.attr.colorOnBackground, GreyOutDecorator.class.getSimpleName());
        normalColor = ColorStateList.valueOf(normalTextColor);
    }

    @Nullable
    @Override
    public ColorStateList getTextColor(@NonNull Context context, int year, int month, int day, boolean valid, boolean selected) {
        return shouldShowGreyOut(year, month, day, valid, selected) ? greyColor : normalColor;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {

    }

    public static final Creator<GreyOutDecorator> CREATOR =
            new Creator<>() {
                @NonNull
                @Override
                public GreyOutDecorator createFromParcel(@NonNull Parcel source) {
                    return new GreyOutDecorator();
                }

                @NonNull
                @Override
                public GreyOutDecorator[] newArray(int size) {
                    return new GreyOutDecorator[size];
                }
            };


    private boolean shouldShowGreyOut(int year, int month, int day, boolean valid, boolean selected) {
        List<Integer> daysOfMonth = datesWithTrajectory.get(new YearMonthCompositeKey(year, month + 1));
        if (daysOfMonth != null) {
            return valid && !selected && !daysOfMonth.contains(day);
        }
        return true;
    }

    public void setDatesWithTrajectory(Map<YearMonthCompositeKey, List<Integer>> datesWithTrajectory) {
        this.datesWithTrajectory = datesWithTrajectory;
    }
}
