Elm.Native.DOMInterface = {};
Elm.Native.DOMInterface.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.DOMInterface = localRuntime.Native.DOMInterface || {};
    if (localRuntime.Native.DOMInterface.values)
    {
        return localRuntime.Native.DOMInterface.values;
    }

    var Task = Elm.Native.Task.make(localRuntime);
    var Maybe = Elm.Maybe.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);

    function getElementPositionInfo(selector) {
        return Task.asyncFunction(function(callback) {
            var elems = document.querySelectorAll(selector);

            if(elems.length === 0) {
                return callback(Task.fail({ctor: 'NodeUndefined'}));
            } else {
                var elemList = [];

                for(var i = 0; i < elems.length; i++) {
                    var elem = elems[i];
                    var elemRect = elem.getBoundingClientRect();
                    var margin = {top: 0, left: 0, bottom: 0, right: 0};
                    if(window.getComputedStyle && typeof window.getComputedStyle === "function") {
                        var computedStyle = window.getComputedStyle(elem); //maybe update for better compatibility
                        margin.top = parseFloat(computedStyle.marginTop);
                        margin.left = parseFloat(computedStyle.marginLeft);
                        margin.bottom = parseFloat(computedStyle.marginBottom);
                        margin.right = parseFloat(computedStyle.marginRight);
                    }
                    elemList.push({
                        top: elemRect.top,
                        left: elemRect.left,
                        width: elemRect.width,
                        height: elemRect.height,
                        offset: {width: elem.offsetWidth, height: elem.offsetHeight},
                        client: {width: elem.clientWidth, height: elem.clientHeight},
                        margin: margin
                    });
                }

                return callback(Task.succeed(List.fromArray(elemList)));
            }
        });
    }

    function scrollElementTo(scrollPosition, selector) {
        return Task.asyncFunction(function(callback) {
            var elems = document.querySelectorAll(selector);

            if(elems.length === 0) {
                return callback(Task.fail({ctor: 'NodeUndefined'}));
            } else {
                elems[0].scrollTop = scrollPosition._1;
                elems[0].scrollLeft = scrollPosition._0;
                return callback(Task.succeed(Utils.Tuple0))
            }
        });
    }

    return localRuntime.Native.DOMInterface.values = {
        getElementPositionInfo: getElementPositionInfo,
        scrollElementTo : F2(scrollElementTo)
    };
};
