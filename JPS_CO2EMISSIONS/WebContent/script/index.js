$( document ).ready(() => {

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
        const model = $('select#model option:selected').val();
        const agent = parseInt($('#agent').val());
        const node = $('#node').val();
        
        console.log(model);
        console.log(agent);
        console.log(node);

        $.ajax({
            method: 'GET',
            dataType: 'json', // dataType of response body
            data: {
                model,agent,node
            },
            url: '/JPS_CO2EMISSIONS/WorldPowerPlant',
            success: [
                    data => {
                    console.log(data);
                }
            ]
        })
    });
});