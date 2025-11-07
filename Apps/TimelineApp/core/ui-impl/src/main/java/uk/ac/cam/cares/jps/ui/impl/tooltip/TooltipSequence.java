package uk.ac.cam.cares.jps.ui.impl.tooltip;

import android.util.Log;
import android.view.View;

import androidx.fragment.app.FragmentActivity;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.ui.base.R;

public class TooltipSequence {

    public static void launch(FragmentActivity activity, View firstTarget, View secondTarget, View thirdTarget, View fourthTarget) {
        TooltipManager tooltipManager = new TooltipManager(activity, () ->
                Log.d("TooltipDebug", "Tooltips finished"));

        tooltipManager.addStep(firstTarget,
                activity.getString(R.string.tooltip_user_menu_title),
                activity.getString(R.string.tooltip_user_menu_message),
                TooltipManager.TooltipStyle.UP);

        tooltipManager.addStep(secondTarget,
                activity.getString(R.string.tooltip_quick_start_title),
                activity.getString(R.string.tooltip_quick_start_message),
                TooltipManager.TooltipStyle.DOWN);

        tooltipManager.addStep(thirdTarget,
                activity.getString(R.string.tooltip_journey_title),
                activity.getString(R.string.tooltip_journey_message),
                TooltipManager.TooltipStyle.DOWN);

        tooltipManager.addStep(fourthTarget,
                activity.getString(R.string.tooltip_about_title),
                activity.getString(R.string.tooltip_about_message),
                TooltipManager.TooltipStyle.UP);

        tooltipManager.start();
    }
}
