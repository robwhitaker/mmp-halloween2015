Elm.Native.Howler = {};
Elm.Native.Howler.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Howler = localRuntime.Native.Howler || {};
    if (localRuntime.Native.Howler.values)
    {
        return localRuntime.Native.Howler.values;
    }

    /*! howler.js v2.0.0-beta | (c) 2013-2015, James Simpson of GoldFire Studios | MIT License | howlerjs.com */
    !function(){"use strict";function e(){try{"undefined"!=typeof AudioContext?n=new AudioContext:"undefined"!=typeof webkitAudioContext?n=new webkitAudioContext:o=!1}catch(e){o=!1}if(!o)if("undefined"!=typeof Audio)try{new Audio}catch(e){t=!0}else t=!0}var n=null,o=!0,t=!1;if(e(),o){var r="undefined"==typeof n.createGain?n.createGainNode():n.createGain();r.gain.value=1,r.connect(n.destination)}var d=function(){this.init()};d.prototype={init:function(){var e=this||u;return e._codecs={},e._howls=[],e._muted=!1,e._volume=1,e.iOSAutoEnable=!0,e.noAudio=t,e.usingWebAudio=o,e.ctx=n,t||e._setupCodecs(),e},volume:function(e){var n=this||u;if(e=parseFloat(e),"undefined"!=typeof e&&e>=0&&1>=e){n._volume=e,o&&(r.gain.value=e);for(var t=0;t<n._howls.length;t++)if(!n._howls[t]._webAudio)for(var d=n._howls[t]._getSoundIds(),a=0;a<d.length;a++){var i=n._howls[t]._soundById(d[a]);i&&i._node&&(i._node.volume=i._volume*e)}return n}return n._volume},mute:function(e){var n=this||u;n._muted=e,o&&(r.gain.value=e?0:n._volume);for(var t=0;t<n._howls.length;t++)if(!n._howls[t]._webAudio)for(var d=n._howls[t]._getSoundIds(),a=0;a<d.length;a++){var i=n._howls[t]._soundById(d[a]);i&&i._node&&(i._node.muted=e?!0:i._muted)}return n},codecs:function(e){return(this||u)._codecs[e]},_setupCodecs:function(){var e=this||u,n=new Audio,o=n.canPlayType("audio/mpeg;").replace(/^no$/,"");return e._codecs={mp3:!(!o&&!n.canPlayType("audio/mp3;").replace(/^no$/,"")),mpeg:!!o,opus:!!n.canPlayType('audio/ogg; codecs="opus"').replace(/^no$/,""),ogg:!!n.canPlayType('audio/ogg; codecs="vorbis"').replace(/^no$/,""),wav:!!n.canPlayType('audio/wav; codecs="1"').replace(/^no$/,""),aac:!!n.canPlayType("audio/aac;").replace(/^no$/,""),m4a:!!(n.canPlayType("audio/x-m4a;")||n.canPlayType("audio/m4a;")||n.canPlayType("audio/aac;")).replace(/^no$/,""),mp4:!!(n.canPlayType("audio/x-mp4;")||n.canPlayType("audio/mp4;")||n.canPlayType("audio/aac;")).replace(/^no$/,""),weba:!!n.canPlayType('audio/webm; codecs="vorbis"').replace(/^no$/,""),webm:!!n.canPlayType('audio/webm; codecs="vorbis"').replace(/^no$/,"")},e},_enableiOSAudio:function(){var e=this||u;if(!n||!e._iOSEnabled&&/iPhone|iPad|iPod/i.test(navigator.userAgent)){e._iOSEnabled=!1;var o=function(){var t=n.createBuffer(1,1,22050),r=n.createBufferSource();r.buffer=t,r.connect(n.destination),"undefined"==typeof r.start?r.noteOn(0):r.start(0),setTimeout(function(){(r.playbackState===r.PLAYING_STATE||r.playbackState===r.FINISHED_STATE)&&(e._iOSEnabled=!0,e.iOSAutoEnable=!1,document.removeEventListener("touchstart",o,!1))},0)};return document.addEventListener("touchstart",o,!1),e}}};var u=new d,a=function(e){var n=this;return e.src&&0!==e.src.length?void n.init(e):void console.error("An array of source files must be passed with any new Howl.")};a.prototype={init:function(e){var t=this;return t._autoplay=e.autoplay||!1,t._ext=e.ext||null,t._html5=e.html5||!1,t._muted=e.mute||!1,t._loop=e.loop||!1,t._pool=e.pool||5,t._preload="boolean"==typeof e.preload?e.preload:!0,t._rate=e.rate||1,t._sprite=e.sprite||{},t._src="string"!=typeof e.src?e.src:[e.src],t._volume=void 0!==e.volume?e.volume:1,t._duration=0,t._loaded=!1,t._sounds=[],t._endTimers={},t._onend=e.onend?[{fn:e.onend}]:[],t._onfaded=e.onfaded?[{fn:e.onfaded}]:[],t._onload=e.onload?[{fn:e.onload}]:[],t._onloaderror=e.onloaderror?[{fn:e.onloaderror}]:[],t._onpause=e.onpause?[{fn:e.onpause}]:[],t._onplay=e.onplay?[{fn:e.onplay}]:[],t._webAudio=o&&!t._html5,"undefined"!=typeof n&&n&&u.iOSAutoEnable&&u._enableiOSAudio(),u._howls.push(t),t._preload&&t.load(),t},load:function(){var e=this,n=null;if(t)return void e._emit("loaderror");"string"==typeof e._src&&(e._src=[e._src]);for(var o=0;o<e._src.length;o++){var r,d;if(e._ext&&e._ext[o]?r=e._ext[o]:(d=e._src[o],r=/^data:audio\/([^;,]+);/i.exec(d),r||(r=/\.([^.]+)$/.exec(d.split("?",1)[0])),r&&(r=r[1].toLowerCase())),u.codecs(r)){n=e._src[o];break}}return n?(e._src=n,new i(e),e._webAudio&&s(e),e):void e._emit("loaderror")},play:function(e){var o=this,t=arguments,r=null;if("number"==typeof e)r=e,e=null;else if("undefined"==typeof e){e="__default";for(var d=0,a=0;a<o._sounds.length;a++)o._sounds[a]._paused&&!o._sounds[a]._ended&&(d++,r=o._sounds[a]._id);1===d?e=null:r=null}var i=r?o._soundById(r):o._inactiveSound();if(!i)return null;if(r&&!e&&(e=i._sprite||"__default"),!o._loaded&&!o._sprite[e])return o.once("load",function(){o.play(o._soundById(i._id)?i._id:void 0)}),i._id;if(r&&!i._paused)return i._id;var _=i._seek>0?i._seek:o._sprite[e][0]/1e3,s=(o._sprite[e][0]+o._sprite[e][1])/1e3-_,l=function(){var t=!(!i._loop&&!o._sprite[e][2]);o._emit("end",i._id),!o._webAudio&&t&&o.stop(i._id).play(i._id),o._webAudio&&t&&(o._emit("play",i._id),i._seek=i._start||0,i._playStart=n.currentTime,o._endTimers[i._id]=setTimeout(l,1e3*(i._stop-i._start)/Math.abs(o._rate))),o._webAudio&&!t&&(i._paused=!0,i._ended=!0,i._seek=i._start||0,o._clearTimer(i._id),i._node.bufferSource=null),o._webAudio||t||o.stop(i._id)};o._endTimers[i._id]=setTimeout(l,1e3*s/Math.abs(o._rate)),i._paused=!1,i._ended=!1,i._sprite=e,i._seek=_,i._start=o._sprite[e][0]/1e3,i._stop=(o._sprite[e][0]+o._sprite[e][1])/1e3,i._loop=!(!i._loop&&!o._sprite[e][2]);var f=i._node;if(o._webAudio){var c=function(){o._refreshBuffer(i);var e=i._muted||o._muted?0:i._volume*u.volume();f.gain.setValueAtTime(e,n.currentTime),i._playStart=n.currentTime,"undefined"==typeof f.bufferSource.start?i._loop?f.bufferSource.noteGrainOn(0,_,86400):f.bufferSource.noteGrainOn(0,_,s):i._loop?f.bufferSource.start(0,_,86400):f.bufferSource.start(0,_,s),o._endTimers[i._id]||(o._endTimers[i._id]=setTimeout(l,1e3*s/Math.abs(o._rate))),t[1]||setTimeout(function(){o._emit("play",i._id)},0)};o._loaded?c():(o.once("load",c),o._clearTimer(i._id))}else{var p=function(){f.currentTime=_,f.muted=i._muted||o._muted||u._muted||f.muted,f.volume=i._volume*u.volume(),f.playbackRate=o._rate,setTimeout(function(){f.play(),t[1]||o._emit("play",i._id)},0)};if(4===f.readyState||!f.readyState&&navigator.isCocoonJS)p();else{var m=function(){o._endTimers[i._id]=setTimeout(l,1e3*s/Math.abs(o._rate)),p(),f.removeEventListener("canplaythrough",m,!1)};f.addEventListener("canplaythrough",m,!1),o._clearTimer(i._id)}}return i._id},pause:function(e){var n=this;if(!n._loaded)return n.once("play",function(){n.pause(e)}),n;for(var o=n._getSoundIds(e),t=0;t<o.length;t++){n._clearTimer(o[t]);var r=n._soundById(o[t]);if(r&&!r._paused){if(r._seek=n.seek(o[t]),r._paused=!0,n._webAudio){if(!r._node.bufferSource)return n;"undefined"==typeof r._node.bufferSource.stop?r._node.bufferSource.noteOff(0):r._node.bufferSource.stop(0),r._node.bufferSource=null}else isNaN(r._node.duration)||r._node.pause();arguments[1]||n._emit("pause",r._id)}}return n},stop:function(e){var n=this;if(!n._loaded)return"undefined"!=typeof n._sounds[0]._sprite&&n.once("play",function(){n.stop(e)}),n;for(var o=n._getSoundIds(e),t=0;t<o.length;t++){n._clearTimer(o[t]);var r=n._soundById(o[t]);if(r&&!r._paused)if(r._seek=r._start||0,r._paused=!0,r._ended=!0,n._webAudio&&r._node){if(!r._node.bufferSource)return n;"undefined"==typeof r._node.bufferSource.stop?r._node.bufferSource.noteOff(0):r._node.bufferSource.stop(0),r._node.bufferSource=null}else r._node&&!isNaN(r._node.duration)&&(r._node.pause(),r._node.currentTime=r._start||0)}return n},mute:function(e,o){var t=this;if(!t._loaded)return t.once("play",function(){t.mute(e,o)}),t;if("undefined"==typeof o){if("boolean"!=typeof e)return t._muted;t._muted=e}for(var r=t._getSoundIds(o),d=0;d<r.length;d++){var a=t._soundById(r[d]);a&&(a._muted=e,t._webAudio&&a._node?a._node.gain.setValueAtTime(e?0:a._volume*u.volume(),n.currentTime):a._node&&(a._node.muted=u._muted?!0:e))}return t},volume:function(){var e,o,t=this,r=arguments;if(0===r.length)return t._volume;if(1===r.length){var d=t._getSoundIds(),a=d.indexOf(r[0]);a>=0?o=parseInt(r[0],10):e=parseFloat(r[0])}else 2===r.length&&(e=parseFloat(r[0]),o=parseInt(r[1],10));var i;if(!("undefined"!=typeof e&&e>=0&&1>=e))return i=o?t._soundById(o):t._sounds[0],i?i._volume:0;if(!t._loaded)return t.once("play",function(){t.volume.apply(t,r)}),t;"undefined"==typeof o&&(t._volume=e),o=t._getSoundIds(o);for(var _=0;_<o.length;_++)i=t._soundById(o[_]),i&&(i._volume=e,t._webAudio&&i._node?i._node.gain.setValueAtTime(e*u.volume(),n.currentTime):i._node&&(i._node.volume=e*u.volume()));return t},fade:function(e,o,t,r){var d=this;if(!d._loaded)return d.once("play",function(){d.fade(e,o,t,r)}),d;d.volume(e,r);for(var u=d._getSoundIds(r),a=0;a<u.length;a++){var i=d._soundById(u[a]);if(i)if(d._webAudio){var _=n.currentTime,s=_+t/1e3;i._volume=e,i._node.gain.setValueAtTime(e,_),i._node.gain.linearRampToValueAtTime(o,s),setTimeout(function(e,t){setTimeout(function(){t._volume=o,d._emit("faded",e)},s-n.currentTime>0?Math.ceil(1e3*(s-n.currentTime)):0)}.bind(d,u[a],i),t)}else{var l=Math.abs(e-o),f=e>o?"out":"in",c=l/.01,p=t/c;!function(){var n=e,t=setInterval(function(e){n+="in"===f?.01:-.01,n=Math.max(0,n),n=Math.min(1,n),n=Math.round(100*n)/100,d.volume(n,e),n===o&&(clearInterval(t),d._emit("faded",e))}.bind(d,u[a]),p)}()}}return d},loop:function(){var e,n,o,t=this,r=arguments;if(0===r.length)return t._loop;if(1===r.length){if("boolean"!=typeof r[0])return o=t._soundById(parseInt(r[0],10)),o?o._loop:!1;e=r[0],t._loop=e}else 2===r.length&&(e=r[0],n=parseInt(r[1],10));for(var d=t._getSoundIds(n),u=0;u<d.length;u++)o=t._soundById(d[u]),o&&(o._loop=e,t._webAudio&&o._node&&(o._node._loop=e));return t},seek:function(){var e,o,t=this,r=arguments;if(0===r.length)o=t._sounds[0]._id;else if(1===r.length){var d=t._getSoundIds(),u=d.indexOf(r[0]);u>=0?o=parseInt(r[0],10):(o=t._sounds[0]._id,e=parseFloat(r[0]))}else 2===r.length&&(e=parseFloat(r[0]),o=parseInt(r[1],10));if("undefined"==typeof o)return t;if(!t._loaded)return t.once("load",function(){t.seek.apply(t,r)}),t;var a=t._soundById(o);if(a){if(!(e>=0))return t._webAudio?a._seek+(t.playing(o)?n.currentTime-a._playStart:0):a._node.currentTime;var i=t.playing(o);i&&t.pause(o,!0),a._seek=e,t._clearTimer(o),i&&t.play(o,!0)}return t},playing:function(e){var n=this,o=n._soundById(e)||n._sounds[0];return o?!o._paused:!1},duration:function(){return this._duration},unload:function(){for(var e=this,n=e._sounds,o=0;o<n.length;o++){n[o]._paused||(e.stop(n[o]._id),e._emit("end",n[o]._id)),e._webAudio||(n[o]._node.src="",n[o]._node.removeEventListener("error",n[o]._errorFn,!1),n[o]._node.removeEventListener("canplaythrough",n[o]._loadFn,!1)),delete n[o]._node,e._clearTimer(n[o]._id);var t=u._howls.indexOf(e);t>=0&&u._howls.splice(t,1)}return _&&delete _[e._src],e=null,null},on:function(e,n,o){var t=this,r=t["_on"+e];return"function"==typeof n&&r.push({id:o,fn:n}),t},off:function(e,n,o){var t=this,r=t["_on"+e];if(n){for(var d=0;d<r.length;d++)if(n===r[d].fn&&o===r[d].id){r.splice(d,1);break}}else r=[];return t},once:function(e,n,o){var t=this,r=function(){n.apply(t,arguments),t.off(e,r,o)};return t.on(e,r,o),t},_emit:function(e,n,o){for(var t=this,r=t["_on"+e],d=0;d<r.length;d++)r[d].id&&r[d].id!==n||setTimeout(function(e){e.call(this,n,o)}.bind(t,r[d].fn),0);return t},_clearTimer:function(e){var n=this;return n._endTimers[e]&&(clearTimeout(n._endTimers[e]),delete n._endTimers[e]),n},_soundById:function(e){for(var n=this,o=0;o<n._sounds.length;o++)if(e===n._sounds[o]._id)return n._sounds[o];return null},_inactiveSound:function(){var e=this;e._drain();for(var n=0;n<e._sounds.length;n++)if(e._sounds[n]._ended)return e._sounds[n].reset();return new i(e)},_drain:function(){var e=this,n=e._pool,o=0,t=0;if(!(e._sounds.length<n)){for(t=0;t<e._sounds.length;t++)e._sounds[t]._ended&&o++;for(t=e._sounds.length-1;t>=0;t--){if(n>=o)return;e._sounds[t]._ended&&(e._webAudio&&e._sounds[t]._node&&e._sounds[t]._node.disconnect(0),e._sounds.splice(t,1),o--)}}},_getSoundIds:function(e){var n=this;if("undefined"==typeof e){for(var o=[],t=0;t<n._sounds.length;t++)o.push(n._sounds[t]._id);return o}return[e]},_refreshBuffer:function(e){var o=this;return e._node.bufferSource=n.createBufferSource(),e._node.bufferSource.buffer=_[o._src],e._node.bufferSource.connect(e._panner?e._panner:e._node),e._node.bufferSource.loop=e._loop,e._loop&&(e._node.bufferSource.loopStart=e._start||0,e._node.bufferSource.loopEnd=e._stop),e._node.bufferSource.playbackRate.value=o._rate,o}};var i=function(e){this._parent=e,this.init()};if(i.prototype={init:function(){var e=this,n=e._parent;return e._muted=n._muted,e._loop=n._loop,e._volume=n._volume,e._muted=n._muted,e._seek=0,e._paused=!0,e._ended=!0,e._id=Math.round(Date.now()*Math.random()),n._sounds.push(e),e.create(),e},create:function(){var e=this,o=e._parent,t=u._muted||e._muted||e._parent._muted?0:e._volume*u.volume();return o._webAudio?(e._node="undefined"==typeof n.createGain?n.createGainNode():n.createGain(),e._node.gain.setValueAtTime(t,n.currentTime),e._node.paused=!0,e._node.connect(r)):(e._node=new Audio,e._errorFn=e._errorListener.bind(e),e._node.addEventListener("error",e._errorFn,!1),e._loadFn=e._loadListener.bind(e),e._node.addEventListener("canplaythrough",e._loadFn,!1),e._node.src=o._src,e._node.preload="auto",e._node.volume=t,e._node.load()),e},reset:function(){var e=this,n=e._parent;return e._muted=n._muted,e._loop=n._loop,e._volume=n._volume,e._muted=n._muted,e._seek=0,e._paused=!0,e._ended=!0,e._sprite=null,e._id=Math.round(Date.now()*Math.random()),e},_errorListener:function(){var e=this;e._node.error&&4===e._node.error.code&&(u.noAudio=!0),e._parent._emit("loaderror",e._id,e._node.error?e._node.error.code:0),e._node.removeEventListener("error",e._errorListener,!1)},_loadListener:function(){var e=this,n=e._parent;n._duration=Math.ceil(10*e._node.duration)/10,0===Object.keys(n._sprite).length&&(n._sprite={__default:[0,1e3*n._duration]}),n._loaded||(n._loaded=!0,n._emit("load")),n._autoplay&&n.play(),e._node.removeEventListener("canplaythrough",e._loadFn,!1)}},o)var _={},s=function(e){var n=e._src;if(_[n])return e._duration=_[n].duration,void c(e);if(/^data:[^;]+;base64,/.test(n)){window.atob=window.atob||function(e){for(var n,o,t="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",r=String(e).replace(/=+$/,""),d=0,u=0,a="";o=r.charAt(u++);~o&&(n=d%4?64*n+o:o,d++%4)?a+=String.fromCharCode(255&n>>(-2*d&6)):0)o=t.indexOf(o);return a};for(var o=atob(n.split(",")[1]),t=new Uint8Array(o.length),r=0;r<o.length;++r)t[r]=o.charCodeAt(r);f(t.buffer,e)}else{var d=new XMLHttpRequest;d.open("GET",n,!0),d.responseType="arraybuffer",d.onload=function(){f(d.response,e)},d.onerror=function(){e._webAudio&&(e._html5=!0,e._webAudio=!1,e._sounds=[],delete _[n],e.load())},l(d)}},l=function(e){try{e.send()}catch(n){e.onerror()}},f=function(e,o){n.decodeAudioData(e,function(e){e&&(_[o._src]=e,c(o,e))},function(){o._emit("loaderror")})},c=function(e,n){n&&!e._duration&&(e._duration=n.duration),0===Object.keys(e._sprite).length&&(e._sprite={__default:[0,1e3*e._duration]}),e._loaded||(e._loaded=!0,e._emit("load")),e._autoplay&&e.play()};"function"==typeof define&&define.amd&&define("howler",function(){return{Howler:u,Howl:a}}),"undefined"!=typeof exports&&(exports.Howler=u,exports.Howl=a),"undefined"!=typeof window&&(window.HowlerGlobal=d,window.Howler=u,window.Howl=a,window.Sound=i)}();

    var Task = Elm.Native.Task.make(localRuntime);
    var Maybe = Elm.Maybe.make(localRuntime);
    var Dict = Elm.Dict.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var List = Elm.Native.List.make(localRuntime);

    var sounds = window.sounds = {};

    function create(soundLabel, audioObj) {
        return Task.asyncFunction(function(callback) {

            var makeSpriteObj = function(dict) {
                if(!dict) return dict;

                var dictArray = List.toArray(Dict.toList(dict));
                var dictObj = {};

                for(var i in dictArray) {
                    var key = dictArray[i]._0;
                    var tuple = dictArray[i]._1;
                    dictObj[key] = [tuple._0, tuple._1, tuple._2];
                }

                return dictObj;
            }

            var soundObject = { soundLabel: soundLabel, playId: Maybe.Nothing };

            var howlObj = {
                src: List.toArray(audioObj.src),
                loop: audioObj.loop._0,
                volume: audioObj.volume._0,
                html5: audioObj.html5._0,
                sprite: makeSpriteObj(audioObj.sprite._0),
                rate: audioObj.rate._0,
                pool: audioObj.pool._0,
                onloaderror: function() { return callback(Task.fail("404 - Audio source not found.")); },
                onload: function() { return callback(Task.succeed(soundObject)); },
            }

            var newHowl = new Howl(howlObj);

            sounds[soundLabel] = newHowl;
        });
    }

    function play(spriteLabel, soundObject) {
        return Task.asyncFunction(function(callback) {
            try {

                // console.log(soundObject.soundLabel, soundObject.playId._0, "start")
                if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

                var sound = sounds[soundObject.soundLabel];
                var playId = soundObject.playId._0;
                var sprite = spriteLabel._0;

                var newPlayId;
                if(sprite != null)
                    newPlayId = sound.play(sprite);
                else
                    newPlayId = (playId == null) ? sound.play() : sound.play(playId);

                return callback(Task.succeed({soundLabel: soundObject.soundLabel, playId: Maybe.Just(newPlayId)}));
            } catch(e) {
                return callback(Task.fail("Failed to play sound."));
            }
        });
    }

    var setWith0 = function(field) {
        return (function(soundObject) {
            return Task.asyncFunction(function(callback) {
                try {
                    // console.log(soundObject.soundLabel, soundObject.playId._0, field)
                    if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

                    var sound = sounds[soundObject.soundLabel];
                    var playId = soundObject.playId._0;

                    (playId == null) ? sound[field]() : sound[field](playId);

                    return callback(Task.succeed(soundObject));
                } catch(e) {
                    return callback(Task.fail("Failure to modify: " + field));
                }
            });
        });
    }

    var setWith1 = function(field) {
        return F2(function(val, soundObject) {
            return Task.asyncFunction(function(callback) {
                try {
                    if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

                    var sound = sounds[soundObject.soundLabel];
                    var playId = soundObject.playId._0;

                    (playId == null) ? sound[field](val) : sound[field](val, playId);

                    return callback(Task.succeed(soundObject));
                } catch(e) {
                    return callback(Task.fail("Failure to modify: " + field));
                }
            });
        });
    }

    var get = function(field) {
        return (function(soundObject) {
            return Task.asyncFunction(function(callback) {
                try {
                    if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

                    var sound = sounds[soundObject.soundLabel];
                    var playId = soundObject.playId._0;

                    var result = (playId == null) ? sound[field]() : sound[field](playId);

                    return callback(Task.succeed(result));
                } catch(e) {
                    return callback(Task.fail("Failure to get: " + field));
                }
            });
        });
    }

    function fade(from, to, duration, soundObject) {
        return Task.asyncFunction(function(callback) {
            try {
                if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

                var sound = sounds[soundObject.soundLabel];
                var playId = soundObject.playId._0;

                (playId == null) ? sound.fade(from, to, duration) : sound.fade(from, to, duration, playId);

                return callback(Task.succeed(soundObject));
            } catch(e) {
                return callback(Task.fail("Failure to fade sound."));
            }
        });
    }

    // function on(eventStr, fn, soundObject) {
    //     return Task.asyncFunction(function(callback) {
    //         try {
    //             if(!sounds[soundObject.soundLabel]) return callback(Task.fail("No such sound object."));

    //             var sound = sounds[soundObject.soundLabel];
    //             var playId = soundObject.playId._0;

    //             var eventHandler = function() { Task.perform(fn()); }

    //             (playId == null) ? sound.on(eventStr, eventHandler) : sound.on(eventStr, eventHandler, playId);

    //             return callback(Task.succeed(soundObject));
    //         } catch(e) {
    //             debugger;
    //             return callback(Task.fail("Failure to add listener."));
    //         }
    //     });
    // }

    return localRuntime.Native.Howler.values = {
        create: F2(create),
        play: F2(play),
        pause: setWith0("pause"),
        stop: setWith0("stop"),
        mute: setWith1("mute"),
        volume: setWith1("volume"),
        seek: setWith1("seek"),
        loop: setWith1("loop"),
        fade: F4(fade),
        isMuted: get("mute"),
        getVolume: get("volume"),
        getSeek: get("seek"),
        isLooping: get("loop"),
        isPlaying: get("playing"),
        getDuration: get("duration")
        // on: F3(on)
    };
};
