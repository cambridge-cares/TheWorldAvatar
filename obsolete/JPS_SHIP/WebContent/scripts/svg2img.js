/**
 * The MIT License (MIT) Copyright (c) 2016 Matt DesLauriers
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */



function loadImage (src, opt, callback) {
    if (typeof opt === 'function') {
        callback = opt;
        opt = null;
    }
    
    var el = document.createElement('img');
    var locked;
    
    el.onload = function onLoaded () {
        if (locked) return;
        locked = true;
        
        if (callback) callback(undefined, el);
    };
    
    el.onerror = function onError () {
        if (locked) return;
        locked = true;
        
        if (callback) callback(new Error('Unable to load "' + src + '"'), el);
    };
    
    if (opt && opt.crossOrigin) {
        el.crossOrigin = opt.crossOrigin;
    }
    
    el.src = src;
    
    return el;
}

var noop = function () {}

function svgToImage (svg, opt, cb) {
    if (typeof opt === 'function') {
        cb = opt
        opt = {}
    }
    cb = cb || noop
    opt = opt || {}
    
    if (typeof window === 'undefined') {
        return bail('window global is undefined; not in a browser')
    }
    
    var DOMURL = getURL()
    if (!DOMURL ||
        typeof DOMURL.createObjectURL !== 'function' ||
        typeof DOMURL.revokeObjectURL !== 'function') {
        return bail('browser does not support URL.createObjectURL')
    }
    
    if (typeof window.Blob === 'undefined') {
        return bail('browser does not support Blob constructor')
    }
    
    if (!Array.isArray(svg)) {
        svg = [ svg ]
    }
    
    var blob
    try {
        blob = new window.Blob(svg, {
            type: 'image/svg+xml;charset=utf-8'
        })
    } catch (e) {
        return bail(e)
    }
    
    var url = DOMURL.createObjectURL(blob)
    
    loadImage(url, opt, function (err, img) {
        DOMURL.revokeObjectURL(url)
        if (err) {
            // try again for Safari 8.0, using simple encodeURIComponent
            // this will fail with DOM content but at least it works with SVG
            var url2 = 'data:image/svg+xml,' + encodeURIComponent(svg.join(''))
            return loadImage(url2, opt, cb)
        }
        
        cb(err, img)
    })
    
    function bail (msg) {
        process.nextTick(function () {
            cb(new Error(msg))
        })
    }
}

function getURL () {
    return window.URL ||
        window.webkitURL ||
        window.mozURL ||
        window.msURL
}