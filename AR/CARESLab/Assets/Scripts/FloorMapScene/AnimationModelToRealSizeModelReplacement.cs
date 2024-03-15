using UnityEngine;

/// <summary>
/// A script attached on animation model and is used to replace the animation model with the real size interactable model. 
/// Because the real size interactable model has physic components attached, 
/// using a model dedicated for animation can eliminate unexpected performance caused by the physics system during the animation. 
/// </summary>
public class AnimationModelToRealSizeModelReplacement : MonoBehaviour
{
    [SerializeField]
    private GameObject realSizeModel;

    /// <summary>
    /// Replace the current animation model with the real size interactable model.
    /// </summary>
    public void Replace()
    {
        GameObject realSizeInstance = Instantiate(realSizeModel);
        realSizeInstance.transform.position = gameObject.transform.position;
        gameObject.SetActive(false);
    }
}
