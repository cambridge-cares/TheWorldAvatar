using System.Collections;
using UnityEngine;

/// <summary>
/// Generate animation for selected model when transiting from floor plan scene to model scene.
/// </summary>
public class ModelAnimationGenerator : MonoBehaviour
{
    [SerializeField]
    private SelectedModel selectedModel;

    private GameObject modelInstance;

    private float startTime = 0.0f;
    private float endTime = 2.0f;

    private void Awake()
    {
        StartCoroutine(waitForLoadFinish());
    }

    private IEnumerator waitForLoadFinish()
    {
        // wait for loading animation from previous scene to finish
        yield return new WaitForSeconds(1.5f);

        StartModelScene();
    }

    private void StartModelScene()
    {
        AnimationClip modelSetupClip = SetModelPositionWithAnimation();
        if (selectedModel.ModelName.Contains("Fumehood"))
        {
            AddSashController(modelSetupClip);
        }
        else if (selectedModel.ModelName.Contains("Cabinet"))
        {
            ReplaceCabinetModel(modelSetupClip);
        }
        GetComponent<LaunchUri>().Launch();
    }

    private AnimationClip SetModelPositionWithAnimation()
    {
        modelInstance = Instantiate(selectedModel.Model);
        modelInstance.transform.position = selectedModel.Position;
        modelInstance.transform.localScale = selectedModel.Scale;
        modelInstance.transform.rotation = selectedModel.Rotation;

        modelInstance.AddComponent<Animation>();
        Animation animation = modelInstance.GetComponent<Animation>();


        AnimationClip clip = new()
        {
            legacy = true,
            name = "fumehood Transform"
        };

        AnimationCurve xPostionCurve = AnimationCurve.Linear(startTime, modelInstance.transform.position.x, endTime, 0);
        clip.SetCurve("", typeof(Transform), "localPosition.x", xPostionCurve);
        AnimationCurve yPostionCurve = AnimationCurve.Linear(startTime, modelInstance.transform.position.y, endTime, 1.5f);
        clip.SetCurve("", typeof(Transform), "localPosition.y", yPostionCurve);
        AnimationCurve zPostionCurve = AnimationCurve.Linear(startTime, modelInstance.transform.position.z, endTime, 2f);
        clip.SetCurve("", typeof(Transform), "localPosition.z", zPostionCurve);

        AnimationCurve xRotationCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localEulerAngles.x, endTime, 0);
        clip.SetCurve("", typeof(Transform), "localEulerAnglesRaw.x", xRotationCurve);
        AnimationCurve yRotationCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localEulerAngles.y, endTime, 0);
        clip.SetCurve("", typeof(Transform), "localEulerAnglesRaw.y", yRotationCurve);
        AnimationCurve zRotationCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localEulerAngles.z, endTime, 0);
        clip.SetCurve("", typeof(Transform), "localEulerAnglesRaw.z", zRotationCurve);

        AnimationCurve xScaleCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localScale.x, endTime, 0.5f);
        clip.SetCurve("", typeof(Transform), "localScale.x", xScaleCurve);
        AnimationCurve yScaleCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localScale.y, endTime, 0.5f);
        clip.SetCurve("", typeof(Transform), "localScale.y", yScaleCurve);
        AnimationCurve zScaleCurve = AnimationCurve.Linear(startTime, modelInstance.transform.localScale.z, endTime, 0.5f);
        clip.SetCurve("", typeof(Transform), "localScale.z", zScaleCurve);

        animation.clip = clip;
        animation.AddClip(clip, clip.name);
        animation.Play(clip.name);

        return clip;
    }

    private void AddSashController(AnimationClip clip)
    {
        clip.AddEvent(new AnimationEvent
        {
            time = endTime - 1f,
            functionName = "UpdateSash"
        });
    }

    private void ReplaceCabinetModel(AnimationClip clip)
    {
        clip.AddEvent(new AnimationEvent
        {
            time = endTime,
            functionName = "Replace"
        });
    }
}
