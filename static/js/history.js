// history.js

$(function  () {
    var olds = $("input[name='old']");
    var diffs = $("input[name='diff']");
    
    function parseDate(node) {
        return parseInt($(node).val());
    }

    function toogleVisibility (node, show) {
        $(node).css("visibility", show ? "visible" : "hidden");
    }

    function selectDiff (evt) {
        var date = parseDate(evt.currentTarget);
        olds.each( function () { toogleVisibility(this, parseDate(this) < date); } );
    }

    function selectOld (evt) {
        var date = parseDate(evt.currentTarget);
        diffs.each( function () { toogleVisibility(this, parseDate(this) > date); } );
    }

    diffs.change(selectDiff)
    olds.change(selectOld);

    diffs.first().attr("checked", "checked");
    $(olds[1]).attr("checked", "checked");
    selectOld({currentTarget: olds[1]});
});

