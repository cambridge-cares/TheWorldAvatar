package uk.ac.cam.cares.jps.ui.tooltip;

import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.graphics.Rect;
import android.graphics.RectF;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.ui.R;

public class TooltipManager {

    private static final String TAG = "TooltipDebug";

    private final Activity activity;
    private final FrameLayout rootOverlay;
    private final List<TooltipStep> steps = new ArrayList<>();
    private final Runnable onComplete;
    private int currentStep = 0;
    private View tooltipView;
    private TooltipOverlayView overlayView;
    private SharedPreferences prefs;

    public TooltipManager(Activity activity, Runnable onComplete) {
        this.activity = activity;
        this.rootOverlay = activity.findViewById(android.R.id.content);
        this.prefs = activity.getSharedPreferences("tooltip_prefs", Context.MODE_PRIVATE);
        this.onComplete = onComplete;
    }

    public void addStep(View anchor, String title, String message, TooltipStyle style) {
        steps.add(new TooltipStep(anchor, null, title, message, style));
    }

    public void addStep(RectF rect, String title, String message, TooltipStyle style) {
        steps.add(new TooltipStep(null, rect, title, message, style));
    }

    public void start() {
        if (prefs.getBoolean("tooltip_skip", false)) {
            Log.d(TAG, "Tooltip skipped by user preference.");
            return;
        }
        if (steps.isEmpty()) {
            Log.d(TAG, "No tooltip steps to show.");
            return;
        }
        currentStep = 0;
        showStep(currentStep);
    }

    private void showStep(int index) {
        if (index >= steps.size()) {
            dismiss();
            return;
        }

        TooltipStep step = steps.get(index);

        if (tooltipView != null) rootOverlay.removeView(tooltipView);
        if (overlayView != null) rootOverlay.removeView(overlayView);

        RectF anchorRect;
        if (step.manualRect != null) {
            anchorRect = step.manualRect;
        } else if (step.anchorView != null) {
            Rect rect = new Rect();
            boolean visible = step.anchorView.getGlobalVisibleRect(rect);
            anchorRect = new RectF(rect);
            if (!visible) {
                dismiss();
                return;
            }
        } else {
            dismiss();
            return;
        }

        overlayView = new TooltipOverlayView(activity);
        overlayView.setHole(anchorRect);
        overlayView.setClickable(false);
        overlayView.setFocusable(false);
        overlayView.setOnTouchListener((v, e) -> false);

        rootOverlay.addView(overlayView, new FrameLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT
        ));

        LayoutInflater inflater = LayoutInflater.from(activity);
        tooltipView = inflater.inflate(R.layout.view_tooltip_callout, rootOverlay, false);

        tooltipView.setClickable(false);
        tooltipView.setFocusable(false);
        tooltipView.setFocusableInTouchMode(false);
        tooltipView.setOnTouchListener((v, event) -> false);

        TextView titleView = tooltipView.findViewById(R.id.tooltip_title);
        TextView messageView = tooltipView.findViewById(R.id.tooltip_message);
        Button nextButton = tooltipView.findViewById(R.id.tooltip_next_button);
        CheckBox dontShowAgain = tooltipView.findViewById(R.id.checkbox_dont_show_again);
        LinearLayout dotContainer = tooltipView.findViewById(R.id.tooltip_dots);
        ImageView arrowUp = tooltipView.findViewById(R.id.tooltip_arrow_up);
        ImageView arrowDown = tooltipView.findViewById(R.id.tooltip_arrow_down);
        ImageView closeButton = tooltipView.findViewById(R.id.tooltip_close);

        titleView.setText(step.title);
        messageView.setText(step.message);
        dontShowAgain.setChecked(false);

        dotContainer.removeAllViews();
        for (int i = 0; i < steps.size(); i++) {
            View dot = new View(activity);
            dot.setBackgroundResource(i == index ? R.drawable.dot_active : R.drawable.dot_inactive);
            LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(20, 20);
            lp.setMargins(6, 0, 6, 0);
            dot.setLayoutParams(lp);
            dotContainer.addView(dot);
        }

        arrowUp.setVisibility(step.style == TooltipStyle.UP ? View.VISIBLE : View.GONE);
        arrowDown.setVisibility(step.style == TooltipStyle.DOWN ? View.VISIBLE : View.GONE);

        tooltipView.getViewTreeObserver().addOnGlobalLayoutListener(() -> {
            FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(
                    ViewGroup.LayoutParams.WRAP_CONTENT,
                    ViewGroup.LayoutParams.WRAP_CONTENT
            );

            float x = anchorRect.centerX() - tooltipView.getWidth() / 2f;
            float y = (step.style == TooltipStyle.UP)
                    ? anchorRect.bottom + 20
                    : anchorRect.top - tooltipView.getHeight() - 20;

            params.leftMargin = Math.max((int) x, 24);
            params.topMargin = Math.max((int) y, 24);
            tooltipView.setLayoutParams(params);

            tooltipView.bringToFront();
            overlayView.bringToFront();
        });

        // Add tooltip and ensure it's drawn above
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            rootOverlay.addView(tooltipView);
            tooltipView.bringToFront();
            overlayView.bringToFront();
            tooltipView.requestLayout();
            overlayView.requestLayout();
        }, 150);

        nextButton.setOnClickListener(v -> {
            if (dontShowAgain.isChecked()) {
                prefs.edit().putBoolean("tooltip_skip", true).apply();
            }
            currentStep++;
            showStep(currentStep);
        });

        closeButton.setOnClickListener(v -> {
            prefs.edit().putBoolean("tooltip_skip", true).apply();
            dismiss();
        });
    }

    private void dismiss() {
        if (tooltipView != null) rootOverlay.removeView(tooltipView);
        if (overlayView != null) rootOverlay.removeView(overlayView);
        if (onComplete != null) onComplete.run();
    }

    private static class TooltipStep {
        View anchorView;
        RectF manualRect;
        String title;
        String message;
        TooltipStyle style;

        TooltipStep(View anchorView, RectF manualRect, String title, String message, TooltipStyle style) {
            this.anchorView = anchorView;
            this.manualRect = manualRect;
            this.title = title;
            this.message = message;
            this.style = style;
        }
    }

    public enum TooltipStyle {
        NONE, UP, DOWN
    }
}
