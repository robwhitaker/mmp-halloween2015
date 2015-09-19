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

    function getElementPositionInfo(selector, index) {
        return Task.asyncFunction(function(callback) {
            var elems = document.querySelectorAll(selector);
            var i = (index >= 0 ? index : elems.length + index);

            if(elems.length === 0 ||  elems[i] === undefined) {
                return callback(Task.fail({ctor: 'NodeUndefined'}));
            } else {
                var elem, elemRect, directParentRect, offsetParentRect;

                elem = elems[i];
                elemRect = elem.getBoundingClientRect();
                directParentRect = elem.parentNode && elem.parentNode.getBoundingClientRect();
                offsetParentRect = elem.offsetParent && elem.offsetParent.getBoundingClientRect();

                return callback(Task.succeed({
                    top : elemRect.top,
                    left : elemRect.left,
                    width : elemRect.width,
                    height : elemRect.height,

                    directParent : (!elem.parentNode ? Maybe.Nothing : Maybe.Just({
                        top : directParentRect.top,
                        left : directParentRect.left,
                        width : directParentRect.width,
                        height : directParentRect.height
                    })),

                    offsetParent : (!elem.offsetParent ? Maybe.Nothing : Maybe.Just({
                        top : offsetParentRect.top,
                        left : offsetParentRect.left,
                        width : offsetParentRect.width,
                        height : offsetParentRect.height
                    }))
                }));
            }
        });
    }

    function scrollElementTo(scrollPosition, selector, index) {
        return Task.asyncFunction(function(callback) {
            var elems = document.querySelectorAll(selector);
            var i = (index >= 0 ? index : elems.length + index);

            if(elems.length === 0 ||  elems[i] === undefined) {
                return callback(Task.fail({ctor: 'NodeUndefined'}));
            } else {
                elems[i].scrollTop = scrollPosition._1;
                elems[i].scrollLeft = scrollPosition._0;
                return callback(Task.succeed(Utils.Tuple0))
            }
        });
    }

    return localRuntime.Native.DOMInterface.values = {
        getElementPositionInfo: F2(getElementPositionInfo),
        scrollElementTo : F3(scrollElementTo)
    };
};
