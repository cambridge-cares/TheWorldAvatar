function showPopupWindow(e, obj) {

    var fromNode = obj.part;
    var fromData = fromNode.data;
    var parentCategory = fromData.category;
    var nextNodeData = {};
    var categoryValue;
    var thisNodeData = {};

    // Set up the model functions
    if (['Optional_MessagePart', 'Mandatory_MessagePart'].includes(parentCategory)) {
        $('#message_part_modal')
            .modal(
                {
                    closeable: true,
                    onDeny: function () {
                        return false;
                    },
                    onApprove: function () {
                        // save the info typed in...
                        var hasValue = $('#instanceSearch').find('.input').find('.prompt').val();
                        var hasDatatype = $('#datatypeSearch').find('.input').find('.prompt').val();
                        var modelReference = $('#classSearch').find('.input').find('.prompt').val();

                        thisNodeData['hasValue'] = hasValue;
                        thisNodeData['hasDatatype'] = hasDatatype;
                        thisNodeData['modelReference'] = modelReference;
                        fromNode['data']['params'] = thisNodeData;
                    },
                    onShow: function () {
                        $('.prompt').val('');
                        if (fromNode['data']['params']) {
                            $('#instanceSearch').find('.input').find('.prompt').val(fromNode['data']['params']['hasValue'])
                            $('#datatypeSearch').find('.input').find('.prompt').val(fromNode['data']['params']['hasDatatype']);
                            $('#classSearch').find('.input').find('.prompt').val(fromNode['data']['params']['modelReference']);
                        }

                        setUpSearchFunction();
                    }

                }
            ).modal('show');
    } else {
        $('#general_modal')
            .modal({
                closable: true,
                onDeny: function () {
                    $('#inputIRI').val('www.theworldavatar.com/');
                    $("#inputHTTPUrl").val('www.theworldavatar.com/');

                    return false;
                },
                onApprove: function () {
                    nextNodeData['fullIRI'] = $('#inputIRI').val();
                    nextNodeData['IRI'] = nextNodeData['fullIRI'].split('/').slice(-1)[0];

                    if (parentCategory === 'Service') {
                        nextNodeData['httpUrl'] = 'http://' + $('#inputHTTPUrl').val()
                    }
                    addNodeAndLink(e, obj, nextNodeData);

                    $('#inputIRI').val('www.theworldavatar.com/');


                },

                onShow: function () {

                    if (parentCategory === 'Service') {
                        $("#httpInput").show();
                    }
                    else {
                        $("#httpInput").hide();
                    }


                    if (DropdownDataGenerator(parentCategory)) {
                        $('.ui.selection.dropdown')
                            .dropdown({
                                    values: DropdownDataGenerator(parentCategory),
                                    onChange: function (value, text, selectedItem) {
                                        nextNodeData['category'] = value;
                                        $('#inputIRI').val('www.theworldavatar.com/' + IRIGenerator(nextNodeData['category']));
                                    }
                                }
                            ).show();
                    } else {
                        $('.ui.selection.dropdown').hide();
                        nextNodeData['category'] = classMap[parentCategory];
                    }


                }
            })
            .modal('show');
    }


    // TODO: make a popup form here, showing a dropdown list


    // When the popup shows up,display the input field
    $('#btnRandom')
        .click(function () {
            console.log('The value is ', nextNodeData['category']);
            $('#inputIRI').val('www.theworldavatar.com/' + IRIGenerator(nextNodeData['category']));
        });

}

function setUpSearchFunction() {
    $('#instanceSearch')
        .search({
            type: 'category',
            minCharacters: 3,
            apiSettings: {
                onResponse: function (response) {
                    return response;
                },
                url: 'http://www.theworldavatar.com/JPS_COMPOSITION/IRISearchAPI?keyword={query}&mode=individual'
            }
        });

    $('#classSearch')
        .search({
            type: 'category',
            minCharacters: 3,
            apiSettings: {
                onResponse: function (response) {

                    return response;
                },
                url: 'http://www.theworldavatar.com/JPS_COMPOSITION/IRISearchAPI?keyword={query}&mode=class'
            }
        });


    $('#datatypeSearch')
        .search({
            minCharacters: 0,
            source: content,
            maxResults: 40
        })


}


