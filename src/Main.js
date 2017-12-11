"use strict";

exports.anim = function (ctx) {
    return function (callback) {
        function loop(timeStamp) {
            callback(ctx)(timeStamp)();
            window.requestAnimationFrame(loop);
        }

        window.requestAnimationFrame(loop);
        return function () {}
    }
}
exports.addEventListener = function (ctx){
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
                // console.log(e);
            }
            canvas.addEventListener(eventType, eventHandler);
            return function () {}
        }
    }
}
