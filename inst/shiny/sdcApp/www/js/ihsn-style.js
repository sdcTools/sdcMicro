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

var VIEW_ID_ANONYMIZE_DISPLAY = '#ui_anonymize.shiny-html-output';

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

  //
  // TABLE_ID_ANONYMIZE_DISPLAY: Full height
  //
  if ($(VIEW_ID_ANONYMIZE_DISPLAY).length > 0){
    // Some tile .offset is not yet available
    if (!$(VIEW_ID_ANONYMIZE_DISPLAY + ' #setupTable .dataTables_scrollBody').offset()) { return; }

    var windowH_1 = $(window).height();
    var tableH_1 = $(VIEW_ID_ANONYMIZE_DISPLAY +' #setupTable .dataTables_scrollBody').height();
    var tableTop_1 =  $(VIEW_ID_ANONYMIZE_DISPLAY +' #setupTable .dataTables_scrollBody').offset().top;
    var margin_1 = 100;
    var targetH_1 = windowH_1 - tableTop_1 - margin_1;
    $(VIEW_ID_ANONYMIZE_DISPLAY + ' #setupTable .dataTables_scrollBody').height(targetH_1);
  }
};

var addFormControlClassForDataTables = function(){
  $( "select" ).addClass( "form-control" );
  $( "input[type=search]" ).addClass( "form-control" );
};

var addFormControlClassForSelectize = function(){
  $(".selectize-input").addClass("form-control");
};

addEvent(window, "resize", function(event) {
  console.log('resized');
  setHeightForTables();
});

$( document ).ready(function() {
  console.log( "ready!" );
  $( ".log" ).text( "Triggered document handler." );

  document.addEventListener("AnonymizeDrawnEvent", function(e) {
    setHeightForTables();
  });

  addFormControlClassForDataTables();
  setHeightForTables();
});

$( document ).ajaxComplete(function() {
  $( ".log" ).text( "Triggered ajaxComplete handler." );
  addFormControlClassForDataTables();
  setHeightForTables();
});

$( document ).load(function() {
    console.log( "load!" );
});

$( document ).change(function() {
    console.log( "change!" );
});

/*
 * Monitor DOM changes
 * https://stackoverflow.com/questions/3219758/detect-changes-in-the-dom
 */
(function (window) {
    var last = +new Date();
    var delay = 100; // default delay

    // Manage event queue
    var stack = [];

    function callback() {
        var now = +new Date();
        if (now - last > delay) {
            for (var i = 0; i < stack.length; i++) {
                stack[i]();
            }
            last = now;
        }
    }

    // Public interface
    var onDomChange = function (fn, newdelay) {
        if (newdelay) delay = newdelay;
        stack.push(fn);
    };

    // Naive approach for compatibility
    function naive() {

        var last = document.getElementsByTagName('*');
        var lastlen = last.length;
        var timer = setTimeout(function check() {

            // get current state of the document
            var current = document.getElementsByTagName('*');
            var len = current.length;

            // if the length is different
            // it's fairly obvious
            if (len != lastlen) {
                // just make sure the loop finishes early
                last = [];
            }

            // go check every element in order
            for (var i = 0; i < len; i++) {
                if (current[i] !== last[i]) {
                    callback();
                    last = current;
                    lastlen = len;
                    break;
                }
            }

            // over, and over, and over again
            setTimeout(check, delay);

        }, delay);
    }

    //
    //  Check for mutation events support
    //

    var support = {};

    var el = document.documentElement;
    var remain = 3;

    // callback for the tests
    function decide() {
        if (support.DOMNodeInserted) {
            window.addEventListener("DOMContentLoaded", function () {
                if (support.DOMSubtreeModified) { // for FF 3+, Chrome
                    el.addEventListener('DOMSubtreeModified', callback, false);
                } else { // for FF 2, Safari, Opera 9.6+
                    el.addEventListener('DOMNodeInserted', callback, false);
                    el.addEventListener('DOMNodeRemoved', callback, false);
                }
            }, false);
        } else if (document.onpropertychange) { // for IE 5.5+
            document.onpropertychange = callback;
        } else { // fallback
            naive();
        }
    }

    // checks a particular event
    function test(event) {
        el.addEventListener(event, function fn() {
            support[event] = true;
            el.removeEventListener(event, fn, false);
            if (--remain === 0) decide();
        }, false);
    }

    // attach test events
    if (window.addEventListener) {
        test('DOMSubtreeModified');
        test('DOMNodeInserted');
        test('DOMNodeRemoved');
    } else {
        decide();
    }

    // do the dummy test
    var dummy = document.createElement("div");
    el.appendChild(dummy);
    el.removeChild(dummy);

    // expose
    window.onDomChange = onDomChange;
})(window);

onDomChange(function(){
  addFormControlClassForSelectize();
  console.log("The Times They Are a-Changin'");
});
