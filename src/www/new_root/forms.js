$(document).ready(function() {

    $('#submitting').hide();
    $("form").submit(function(){

        $('button[type=submit]').attr('disabled', 'disabled');
        $('button[type=submit]').hide();
        $('#submitting').show();
    });
});