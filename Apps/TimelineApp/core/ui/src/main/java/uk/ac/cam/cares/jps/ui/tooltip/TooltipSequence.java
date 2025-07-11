package uk.ac.cam.cares.jps.ui.tooltip;

import android.app.Activity;
import android.util.Log;
import android.view.View;

public class TooltipSequence {
    public static void launch(Activity activity, View firstTarget, View secondTarget, View thirdTarget, View fourthTarget) {
        TooltipManager tooltipManager = new TooltipManager(activity, () ->
                Log.d("TooltipDebug", "Tooltips finished"));

        tooltipManager.addStep(firstTarget,
                "User Menu",
                "Tap here to access your account settings and sensor preferences",
                TooltipManager.TooltipStyle.UP);

        tooltipManager.addStep(secondTarget,
                "Quick Start Recording",
                "Instantly toggle all sensors.\nWant control? Customize your selection in Sensor Settings.",
                TooltipManager.TooltipStyle.UP);

        tooltipManager.addStep(thirdTarget,
                "Track your journey here",
                "Swipe up or down to see where you’ve been and how you got there.",
                TooltipManager.TooltipStyle.DOWN);

        tooltipManager.addStep(fourthTarget,
                "Need this info again?",
                "Tap the menu and select 'About'.",
                TooltipManager.TooltipStyle.UP);

        tooltipManager.start();
    }
}
