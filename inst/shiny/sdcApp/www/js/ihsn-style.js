/*
 * Custom JS
 */
$( document ).ready(function() {
    console.log( "ready!" );
    // alert("Ready");
});


$( document ).ajaxComplete(function() {
  $( ".log" ).text( "Triggered ajaxComplete handler." );
  $( "select" ).addClass( "form-control" );
});
