/*
 * Custom JS
 */


 /* Get the Resize Event (No jQuery) */
var addEvent = function(object, type, callback) {
    if (object == null || typeof(object) == 'undefined') return;
    if (object.addEventListener) {
        object.addEventListener(type, callback, false);
    } else if (object.attachEvent) {
        object.attachEvent("on" + type, callback);
    } else {
        object["on"+type] = callback;
    }
};

var VIEW_ID_MICRODATA_DISPLAY = '#ui_show_microdata';
var VIEW_ID_MICRODATA_SETVALUESNA = '#ui_set_to_na';

var setHeightForTables = function(){
  //
  // TABLE_ID_MICRODATA_DISPLAY: Full coverage
  //
  if ($(VIEW_ID_MICRODATA_DISPLAY).length > 0){
    var windowH = $(window).height();
    var tableH = $('.dataTables_scrollBody').height();
    var tableTop =  $(".dataTables_scrollBody").offset().top;
    var margin = 80;
    var targetH = windowH - tableTop - margin;
    $(VIEW_ID_MICRODATA_DISPLAY + ' .dataTables_scrollBody').height(targetH);
  }
  //
  // TABLE_ID_MICRODATA_SETVALUESNA: No Action
  //
}

addEvent(window, "resize", function(event) {
  console.log('resized');
  setHeightForTables();
});

$( document ).ready(function() {
    console.log( "ready!" );
    setHeightForTables();
});

$( document ).ajaxComplete(function() {
  $( ".log" ).text( "Triggered ajaxComplete handler." );
  $( "select" ).addClass( "form-control" );
  $( "input[type=search]" ).addClass( "form-control" );
  setHeightForTables();
});
