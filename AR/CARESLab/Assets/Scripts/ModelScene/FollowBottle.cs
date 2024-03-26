using UnityEngine;

/// <summary>
/// A script use to control the current gameobject to follow another.
/// </summary>
public class FollowBottle : MonoBehaviour
{
    /// <summary>
    /// The target gameobject
    /// </summary>
    public GameObject Target;

    private Vector3 prevTargetPosition;

    private void Start()
    {
        prevTargetPosition = Target.transform.position;
    }

    // Update is called once per frame
    void Update()
    {
        gameObject.transform.position += Target.transform.position - prevTargetPosition;
        prevTargetPosition = Target.transform.position;
    }
}
