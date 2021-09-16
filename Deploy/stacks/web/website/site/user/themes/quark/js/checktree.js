(function($){
    $.fn.checktree = function(){
        $(':checkbox').on('click', function (event){
            event.stopPropagation();
            var clk_checkbox = $(this),
            chk_state = clk_checkbox.is(':checked'),
            parent_li = clk_checkbox.closest('li'),
            parent_uls = parent_li.parents('ul');
            parent_li.find(':checkbox').prop('checked', chk_state);
            parent_uls.each(function(){
                parent_ul = $(this);
                parent_state = (parent_ul.find(':checkbox').length == parent_ul.find(':checked').length); 
                parent_ul.siblings(':checkbox').prop('checked', parent_state);
            });
         });
    };
}(jQuery));
