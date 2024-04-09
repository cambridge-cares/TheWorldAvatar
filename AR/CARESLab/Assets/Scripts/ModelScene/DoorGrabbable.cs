using UnityEngine;

/// <summary>
/// A script attachd on door grabbable game object as part of the door opening modeling. See also <see cref="FollowPhysics"/>. 
/// </summary>
public class DoorGrabbable : MonoBehaviour
{
    public Transform HandlerTransform;

    /// <summary>
    /// Reset the door grabbable game object position and the door's velocity and angular speed when manipulation ends.
    /// </summary>
    /// <remarks>
    /// Use as a callback for <see herf="https://learn.microsoft.com/en-us/dotnet/api/mixedreality.toolkit.statefulinteractable?view=mrtkcore-3.0" langword="StatefulInteractable"/> when manipulation ended.
    /// </remarks>
    public void Reset()
    {
        transform.position = HandlerTransform.transform.position;
        transform.rotation = HandlerTransform.transform.rotation;

        // reset the velocity and angular speed so the door will not bounce around when hitting with the controller collider.
        Rigidbody rbHandler = HandlerTransform.GetComponent<Rigidbody>();
        rbHandler.velocity = Vector3.zero;
        rbHandler.angularVelocity = Vector3.zero;
    }
}
