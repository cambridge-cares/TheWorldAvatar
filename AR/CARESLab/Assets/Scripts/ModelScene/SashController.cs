using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using UnityEngine;
using UnityEngine.Networking;

/// <summary>
/// Control the sash position of the model based on real world position.
/// </summary>
public class SashController : MonoBehaviour
{
    [SerializeField]
    private GameObject sash;

    [SerializeField]
    private SelectedModel selectedFumeHood;

    private const float yFullyOpen = 0.665f;
    private const float yClosed = 0f;

    private const float startTime = 0;
    private const float sashSpeed = (float)(3.0 / 0.665);

    private float prevSashPercentage = 0;
    private DateTime prevUpdateTime = DateTime.Now;

    // Update is called once per frame
    void Update()
    {
        if ((System.DateTime.Now - prevUpdateTime).TotalSeconds >= 2)
        {
            UpdateSash();
            prevUpdateTime = System.DateTime.Now;
        }

    }

    /// <summary>
    /// Get sash opening data from fh-sash-and-occupancy-agent and control the position of the sash model
    /// </summary>
    public void UpdateSash()
    {
        StartCoroutine(GetSashOpening());
    }

    IEnumerator GetSashOpening()
    {
        string url = Config.FhSashAndOccupancyAgentUrl + "?deviceIri=" + selectedFumeHood.Iri;
        using UnityWebRequest www = UnityWebRequest.Get(url);
        yield return www.SendWebRequest();

        if (www.result == UnityWebRequest.Result.ConnectionError || www.result == UnityWebRequest.Result.ProtocolError)
        {
            Debug.LogError("Error: " + www.error);
        }
        else
        {
            string jsonResponse = www.downloadHandler.text;

            JObject jObject = JObject.Parse(jsonResponse);

            float currentSashPercentage = float.Parse((string)jObject["sash"]) / 100;
            if (Math.Abs(currentSashPercentage - prevSashPercentage) > 0.01)
            {
                Debug.Log("Sash percentage updated to: " + jObject["sash"]);
                ChangeSashPosition(currentSashPercentage);
                prevSashPercentage = currentSashPercentage;
            }
            // discard the value if not greater than 1% of change
        }
    }

    private void ChangeSashPosition(float sashPercentage)
    {
        sashPercentage = Math.Clamp(sashPercentage, 0f, 1f);
        float newY = (yFullyOpen - yClosed) * sashPercentage + yClosed;
        float sashMoveDistance = Math.Abs(newY - sash.transform.localPosition.y);

        Animation animation = sash.GetComponent<Animation>();
        if (animation == null)
        {
            sash.AddComponent<Animation>();
        }

        AnimationClip clip = new()
        {
            legacy = true,
            name = "sash Transform"
        };
        AnimationCurve xPostionCurve = AnimationCurve.Linear(startTime, sash.transform.localPosition.x, sashMoveDistance * sashSpeed, sash.transform.localPosition.x);
        clip.SetCurve("", typeof(Transform), "localPosition.x", xPostionCurve);
        AnimationCurve yPostionCurve = AnimationCurve.Linear(startTime, sash.transform.localPosition.y, sashMoveDistance * sashSpeed, newY);
        clip.SetCurve("", typeof(Transform), "localPosition.y", yPostionCurve);
        AnimationCurve zPostionCurve = AnimationCurve.Linear(startTime, sash.transform.localPosition.z, sashMoveDistance * sashSpeed, sash.transform.localPosition.z);
        clip.SetCurve("", typeof(Transform), "localPosition.z", zPostionCurve);

        animation.clip = clip;
        animation.AddClip(clip, clip.name);
        animation.Play(clip.name);
    }
}
