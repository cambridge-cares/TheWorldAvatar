using UnityEngine;

/// <summary>
/// Script makes the current gameobject follows the target. Part of door opening modeling. See also <see cref="DoorGrabbable"/>.
/// </summary>
/// <remarks>
/// The door opening model consists of three gameobjects, the door, (invisible) door control and (invisible) door grab area. 
/// Hinge joint is attached on the door with cabinet body as the connected body, and this gives the door hinge movement and follow the cabinet body when the body is moved around. 
/// Fixed joint is attached on door control with the door as connected body, so the door control and the door will move together.
/// The door control's position is updated by this script, and it will follow the door grab area which is grabbale by user.
/// Therefore when user grabs the grab area around, the door control will follow the grab area and the door change position also due to the fixed joint.
/// </remarks>
public class FollowPhysics : MonoBehaviour
{
    /// <summary>
    /// The target for the current gameobject to follow. It is usually the door grab area.
    /// </summary>
    public Transform target;
    Rigidbody rigidBody;

    /// <summary>
    /// The taget distance limit is the maximum accepted distance for the grab area.
    /// The door opening speed is related to the distance of the grab area. Further away the grab area is, faster the door will be open.
    /// It is used to prevent the door moves too fast when the grab area is moved too far away from the original position.
    /// </summary>
    public float TargetDistanceLimit = 0.05f;

    // Start is called before the first frame update
    void Start()
    {
        rigidBody = GetComponent<Rigidbody>();
    }

    // Update is called once per frame
    void Update()
    {
        Vector3 movement = target.transform.position - gameObject.transform.position;
        if (movement.magnitude > TargetDistanceLimit)
        {
            movement = movement.normalized * TargetDistanceLimit;
        }

        rigidBody.MovePosition(gameObject.transform.position + movement);
    }
}
