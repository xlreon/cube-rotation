"use strict";

exports.getFaceVert = function (vertices ) {
    return function (faces) {
        return function(index) {
            return vertices[faces[index]]
        }
    }
}

exports.getFace = function (faces) {
    return function (index) {
        return faces[index];
    }
}

exports.animate = function (ctx) {
    return function (callback) {
        function loop () {
            callback(ctx)();
            window.requestAnimationFrame(loop);
        }

        window.requestAnimationFrame(loop);
        return function () {}
    }
}

exports.addEventListener = function (canvas) {
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
                console.log(e);
            }
            // console.log(canvas);
            // canvas.addEventListner(eventType, eventHandler);
            document.getElementById("canvas").addEventListener(eventType,eventHandler);
            return function () {};  
        }
    }
} 

// exports.changeColor = function(callback) {
//     return function(color) {
//         callback(color);
//         return function () {};
//     }
// }

exports.addBtnListener= function(btnId) {
    return function(color) {
        return function (callback) {
            function eventHandler() {
                callback(color);
            }
    document.getElementById(btnId).addEventListener("click",eventHandler);
    return function () {};
}
}
}

exports.resetDefault = function(btnId) {
    return function(callback1) {
        return function(callback2) {
            return function(callback3) {
                return function(callback4) {
                    return function(callback5) {

                        function eventHandler() {
                            callback1('#000000');
                            callback2('#FFFFFF');
                            callback3();
                            callback4();
                            callback5();
                        }
                        document.getElementById(btnId).addEventListener("click",eventHandler);
                    }
                }
            }
        }
    }
}