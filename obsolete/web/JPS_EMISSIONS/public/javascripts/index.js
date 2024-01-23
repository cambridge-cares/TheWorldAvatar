const fadeThumbnailIn = imageObjArray => {
    // thumbnailElements = document.getElementsByClassName('thumbnail');
    //
    // Array.prototype.forEach.call(thumbnailElements, el => {
    //     el.hide(0, () => {
    //         el.fadeIn(700);
    //     })
    // });

    $('.thumbnail').addClass('load');
    for(let imageObj of imageObjArray) {
        fadeImageIn(imageObj.element, imageObj.source);
    }
};

const fadeImageIn = (imageElement, imageSource) => {
    imageElement.hide(0, () => {
        imageElement.attr('src', imageSource);
        imageElement.fadeIn(700);
    })
};


$( document ).ready(() => {

    //
    // const mask = $('<div class="bgmask"></div>');
    //
    // thumbnailElementsArray = $(".thumbnail");
    // $.each(thumbnailElementsArray, (key, thumbnail) => {
    //     thumbnail.addEventListener('click', () => {
    //         $('body').append(mask);
    //     })
    // });

    const toggleDisplay = elemId => {
        let x = document.getElementById(elemId);
        if (x.style.display !== 'block') {
            x.style.display = 'block';
        } else {
            x.style.display = 'none';
        }
    };

    $("#readme-button").click(function() {
        toggleDisplay("readme-text");
    });

    document.addEventListener("click", function(evt) {
        var readmeButtonElement = document.getElementById('readme-button'),
            readmeTextElement = document.getElementById('readme-text'),
            targetElement = evt.target;  // clicked element

        if (targetElement == readmeButtonElement || targetElement == readmeTextElement) {
            return; //readme-button or readme-text is clicked. do nothing.
        }

        if(readmeTextElement.style.display === 'block') {
            readmeTextElement.style.display = 'none';
        }
    });

    $('#start').on('click', event => {
        event.preventDefault();
        const carbonPrice = $('select#carbon-price option:selected').val();
        //const technologyLearningRate = $('select#technology-learning-rate option:selected').val();
		const technologyLearningRate = 'low';

        $('#annual').attr('src', `/images/${carbonPrice}_carbon_annual.png`);
        $('#avoidance_cost').attr('src', `/images/${carbonPrice}_carbon_avoidance_cost.png`);
        $('#lock').attr('src', `/images/${carbonPrice}_carbon_lock.png`);
        $('#price').attr('src', `/images/${carbonPrice}_carbon_price.png`);

        // let imageObjArray = [];
        //
        // const FOSSIL_CAP = "/images/1_top_ten_emission_countries_fossil_ele_cap.png";
        // const FOSSIL_CAP_PER = "/images/1_top_ten_emission_countries_fossil_ele_cap_per.png";
        // const FOSSIL_GEN = "/images/1_top_ten_emission_countries_fossil_ele_gen.png";
        // const FOSSIL_GEN_PER = "/images/1_top_ten_emission_countries_fossil_ele_gen_per.png";
        //
        // const imageTopLeft = $("#top-left");
        // imageObjArray.push(
        //     {
        //         element: imageTopLeft,
        //         source: FOSSIL_CAP
        //     }
        // );
        //
        // const imageTopRight = $("#top-right");
        // imageObjArray.push(
        //     {
        //         element: imageTopRight,
        //         source: FOSSIL_CAP_PER
        //     }
        // );
        //
        // const imageBottomLeft = $("#bottom-left");
        // imageObjArray.push(
        //     {
        //         element: imageBottomLeft,
        //         source: FOSSIL_GEN
        //     }
        // );
        //
        // const imageBottomRight = $("#bottom-right");
        // imageObjArray.push(
        //     {
        //         element: imageBottomRight,
        //         source: FOSSIL_GEN_PER
        //     }
        // );
        //
        // fadeThumbnailIn(imageObjArray);

        $.ajax({
            method: 'POST',
            dataType: 'json', // dataType of response body
            data: {
                carbonPrice,
                technologyLearningRate
            },
            url: '/JPS_EMISSIONS/TestEndPoint',
            success: [
                    data => {
                    console.log(data);

                    $('#annual-baseline').attr('src', '/images/annual_baseline.png');
                }
            ]
        })
    });

    // $.ajax({
    //     dataType: "json",
    //     url: "/JPS_EMISSIONS/HeatMap",
    //     success: [
    //         data => {
    //             const pathsObj = data;
    //             let imageObjArray = [];
    //
    //             const imageTopLeft = $("#top-left");
    //             // fadeImageIn(imageTopLeft, pathsObj.fossilCap);
    //             imageObjArray.push(
    //                 {
    //                     element: imageTopLeft,
    //                     source: pathsObj.fossilCap
    //                 }
    //             );
    //
    //             const imageTopRight = $("#top-right");
    //             // fadeImageIn(imageTopRight, pathsObj.fossilCapPer);
    //             imageObjArray.push(
    //                 {
    //                     element: imageTopRight,
    //                     source: pathsObj.fossilCapPer
    //                 }
    //             );
    //
    //             const imageBottomLeft = $("#bottom-left");
    //             // fadeImageIn(imageBottomLeft, pathsObj.fossilGen);
    //             imageObjArray.push(
    //                 {
    //                     element: imageBottomLeft,
    //                     source: pathsObj.fossilGen
    //                 }
    //             );
    //
    //             const imageBottomRight = $("#bottom-right");
    //             // fadeImageIn(imageBottomRight, pathsObj.fossilGenPer);
    //             imageObjArray.push(
    //                 {
    //                     element: imageBottomRight,
    //                     source: pathsObj.fossilGenPer
    //                 }
    //             );
    //
    //             fadeThumbnailIn(imageObjArray)
    //         }]
    // });

    // $.ajax({
    //     dataType: "json",
    //     url: "/JPS_EMISSIONS/CommittedEmission",
    //     success: [
    //         data => {
    //             let pathsObj = data;
    //
    //             const imageTest = $("#bottom-left-3");
    //             fadeImageIn(imageTest, pathsObj.carbon0Annual);
    //         }]
    // });

    // $.ajax({
    //     dataType: "json",
    //     url: "/JPS_EMISSIONS/GenerationCost",
    //     success: [
    //         data => {
    //             let pathsObj = data;
    //         }]
    // });

    // $.ajax({
    //     dataType: "json",
    //     url: "/JPS_EMISSIONS/AvoidanceCost",
    //     success: [
    //         data => {
    //             let pathsObj = data;
    //         }]
    // });

    // $.ajax({
    //     dataType: "json",
    //     url: "/JPS_EMISSIONS/ParallelCoordinates",
    //     success: [
    //         data => {
    //             let pathsObj = data;
    //         }]
    // });
});