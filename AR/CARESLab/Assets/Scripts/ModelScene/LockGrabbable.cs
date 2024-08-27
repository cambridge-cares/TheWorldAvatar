using UnityEngine;

/// <summary>
/// A script used to model cabinet lock. 
/// When the lock handle is pulled more than 75 degree and both doors are closed, the cabinet is considered to be unlocked, otherwise the cabinet is locked.
/// When locked, the left and the right door of the cabinet are not grabbable. They are grabbable when unlocked.
/// The state of the lock is checked every frame.
/// </summary>
public class LockGrabbable : DoorGrabbable
{
    public HingeJoint CurrentHingeJoint;
    public HingeJoint RightDoorHingeJoint;
    public HingeJoint LeftDoorHingeJoint;
    public GameObject RightDoorGrabArea;
    public GameObject RightDoorControl;
    public GameObject LeftDoorControl;
    public GameObject HandCoach;

    private bool isManipulating = false;

    private void Update()
    {
        if (CurrentHingeJoint.angle > 75 || RightDoorHingeJoint.angle < -0.8 || LeftDoorHingeJoint.angle > 0.3)
        {
            // is unlocked
            if (isManipulating)
            {
                RightDoorGrabArea.transform.position = gameObject.transform.position;
            }

            if (!RightDoorControl.activeSelf)
            {
                RightDoorControl.SetActive(true);
            }

            if (!LeftDoorControl.activeSelf)
            {
                LeftDoorControl.SetActive(true);
            }
            HandCoach.SetActive(false);
        }
        else
        {
            // is locked
            if (RightDoorControl.activeSelf)
            {
                RightDoorControl.SetActive(false);
            }

            if (LeftDoorControl.activeSelf)
            {
                LeftDoorControl.SetActive(false);
            }
        }
    }

    /// <summary>
    /// Set whether the lock is being manipulated. Used as callback for <see herf="https://learn.microsoft.com/en-us/dotnet/api/mixedreality.toolkit.statefulinteractable?view=mrtkcore-3.0" langword="StatefulInteractable"/> 
    /// </summary>
    /// <param name="isManipulating">Whether the current gameobject is being manipulated</param>
    public void SetManipulation(bool isManipulating)
    {
        this.isManipulating = isManipulating;
    }
}
