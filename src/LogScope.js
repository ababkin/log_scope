// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            var f = t.f.f;
            t.f = 0;
            t.f = f();
        }
        return t.f;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = 0;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2){return new F(function(){return A(_2,[_1]);});},_3=function(_4){var _5=jsRound(_4),_6=_5;return [0,_6>>>0&255];},_7=function(_8){var _9=B(A(_8,[_])),_a=_9;return E(_a);},_b=function(_c){return new F(function(){return _7(function(_){var _=0;return new F(function(){return eval(_c);});});});},_d=new T(function(){return B(_b("(function(b,i){return b.getUint8(i);})"));}),_e=function(_f){return function(_g,_){var _h=B(A(new T(function(){return B(A(_d,[E(_f)]));}),[E(E(_g)[1]),_])),_i=_h;return new T(function(){return B(_3(_i));});};},_j=new T(function(){return B(unCStr("Wrong magic byte for ServerException"));}),_k=[0,_j],_l=function(_m,_n){var _o=E(_m);return _o[0]==0?E(_n):[1,_o[1],new T(function(){return B(_l(_o[2],_n));})];},_p=function(_q,_r){var _s=jsShowI(_q),_t=_s;return new F(function(){return _l(fromJSStr(_t),_r);});},_u=[0,41],_v=[0,40],_w=function(_x,_y,_z){if(_y>=0){return new F(function(){return _p(_y,_z);});}else{return _x<=6?B(_p(_y,_z)):[1,_v,new T(function(){var _A=jsShowI(_y),_B=_A;return B(_l(fromJSStr(_B),[1,_u,_z]));})];}},_C=[0],_D=function(_E){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_w(9,_E,_C));}))));});},_F=function(_G){var _H=jsRound(_G),_I=_H;return [0,_I];},_J=new T(function(){return B(_b("(function(b,i){return b.getInt32(i,true);})"));}),_K=function(_L){return function(_M,_){var _N=B(A(new T(function(){return B(A(_J,[E(_L)]));}),[E(E(_M)[1]),_])),_O=_N;return new T(function(){return B(_F(_O));});};},_P=function(_Q,_R){return [1,[0,new T(function(){return [0,E(_R)[1]+4|0];}),new T(function(){var _S=B(_7(function(_){var _=0;return new F(function(){return A(_K,[_Q,_R,_]);});}))[1];if(_S>>>0>1114111){var _T=B(_D(_S));}else{var _T=[0,_S];}var _U=_T,_V=_U,_W=_V;return _W;})]];},_X=[0,_P],_Y=0,_Z=new T(function(){return [0,"(function(a,x) {a.push(x);})"];}),_10=function(_11,_12){return [0,_11,_12];},_13=function(_14){return E(_14);},_15=new T(function(){return B(_10(_13,_13));}),_16=[0,4],_17=new T(function(){return [0,"Int32Array"];}),_18=function(_19){return E(E(_19)[2]);},_1a=new T(function(){return [0,"window[\'toABle\']"];}),_1b=new T(function(){return B(_b(E(_1a)[1]));}),_1c=function(_1d,_1e){return function(_1f){return function(_1g,_){return new F(function(){return A(new T(function(){return B(A(new T(function(){return B(A(_1b,[E(E(_1e)[1])]));}),[E(E(_1f)[1])]));}),[B(A(new T(function(){return B(_18(_1d));}),[_1g])),_]);});};};},_1h=function(_1i){return function(_1j,_){var _1k=B(A(_b,[E(_Z)[1],E(_1j),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_17,_16,new T(function(){return E(E(_1i)[1]);}),_]);});}));})),_])),_1l=_1k;return _Y;};},_1m=function(_1n){return [0,B(_1h(new T(function(){return [0,E(_1n)[1]];})))];},_1o=[0,_X,_1m],_1p=function(_1q,_1r){return [1,[0,_1r,_C]];},_1s=[0,1],_1t=function(_1u){return I_toInt(_1u)>>>0;},_1v=function(_1w){var _1x=E(_1w);return _1x[0]==0?_1x[1]>>>0:B(_1t(_1x[1]));},_1y=function(_1z){return [0,B(_1v(_1z))];},_1A=function(_1B,_1C){return [1,new T(function(){return B(_1y(_1B));}),_1C];},_1D=function(_1E){return [0,_1E];},_1F=function(_1G){return [1,I_fromInt(_1G)];},_1H=function(_1I){var _1J=_1I&4294967295;return _1J<0?B(_1F(_1I)):B(_1D(_1J));},_1K=[0,0],_1L=function(_1M,_1N){var _1O=E(_1M);if(!_1O[0]){var _1P=_1O[1],_1Q=E(_1N);return _1Q[0]==0?_1P>=_1Q[1]:I_compareInt(_1Q[1],_1P)<=0;}else{var _1R=_1O[1],_1S=E(_1N);return _1S[0]==0?I_compareInt(_1R,_1S[1])>=0:I_compare(_1R,_1S[1])>=0;}},_1T=function(_1U,_1V){var _1W=E(_1U);if(!_1W[0]){var _1X=_1W[1],_1Y=E(_1V);return _1Y[0]==0?_1X>_1Y[1]:I_compareInt(_1Y[1],_1X)<0;}else{var _1Z=_1W[1],_20=E(_1V);return _20[0]==0?I_compareInt(_1Z,_20[1])>0:I_compare(_1Z,_20[1])>0;}},_21=function(_22,_23){var _24=E(_22);if(!_24[0]){var _25=_24[1],_26=E(_23);return _26[0]==0?_25<_26[1]:I_compareInt(_26[1],_25)>0;}else{var _27=_24[1],_28=E(_23);return _28[0]==0?I_compareInt(_27,_28[1])<0:I_compare(_27,_28[1])<0;}},_29=function(_2a,_2b){while(1){var _2c=E(_2a);if(!_2c[0]){var _2d=_2c[1],_2e=E(_2b);if(!_2e[0]){var _2f=_2e[1],_2g=addC(_2d,_2f);if(!E(_2g[2])){return [0,_2g[1]];}else{_2a=[1,I_fromInt(_2d)];_2b=[1,I_fromInt(_2f)];continue;}}else{_2a=[1,I_fromInt(_2d)];_2b=_2e;continue;}}else{var _2h=E(_2b);if(!_2h[0]){_2a=_2c;_2b=[1,I_fromInt(_2h[1])];continue;}else{return [1,I_add(_2c[1],_2h[1])];}}}},_2i=function(_2j,_2k,_2l,_2m,_2n){if(!B(_1L(_2m,_1K))){var _2o=function(_2p){if(!B(_21(_2p,_2n))){return new F(function(){return A(_2j,[_2p,new T(function(){return B(_2o(B(_29(_2p,_2m))));})]);});}else{return E(_2k);}};return new F(function(){return _2o(_2l);});}else{var _2q=function(_2r){if(!B(_1T(_2r,_2n))){return new F(function(){return A(_2j,[_2r,new T(function(){return B(_2q(B(_29(_2r,_2m))));})]);});}else{return E(_2k);}};return new F(function(){return _2q(_2l);});}},_2s=function(_2t,_2u){return new F(function(){return _2i(_1A,_C,B(_1H(_2t)),_1s,B(_1H(_2u)));});},_2v=function(_2w){return E(E(_2w)[1]);},_2x=function(_2y){var _2z=jsRound(_2y),_2A=_2z;return [0,_2A>>>0];},_2B=new T(function(){return B(_b("(function(b,i){return b.getUint32(i,true);})"));}),_2C=function(_2D){return function(_2E,_){var _2F=B(A(new T(function(){return B(A(_2B,[E(_2D)]));}),[E(E(_2E)[1]),_])),_2G=_2F;return new T(function(){return B(_2x(_2G));});};},_2H=function(_2I){return function(_2J,_2K){var _2L=B(_2s(1,B(_7(function(_){var _=0;return new F(function(){return A(_2C,[_2J,_2K,_]);});}))[1]));if(!_2L[0]){return [1,[0,new T(function(){return [0,E(_2K)[1]+4|0];}),_C]];}else{var _2M=E(new T(function(){return B(_2v(_2I));}))[1],_2N=B(A(_2M,[_2J,new T(function(){return [0,E(_2K)[1]+4|0];})]));if(!_2N[0]){return [0,_2N[1]];}else{var _2O=E(_2N[1]),_2P=function(_2Q){var _2R=E(_2Q);return _2R[0]==0?_1p:function(_2S,_2T){var _2U=B(A(_2M,[_2S,_2T]));if(!_2U[0]){return [0,_2U[1]];}else{var _2V=E(_2U[1]),_2W=B(A(E(new T(function(){return [0,B(_2P(_2R[2]))];}))[1],[_2S,_2V[1]]));if(!_2W[0]){return E(_2W);}else{var _2X=E(_2W[1]);return [1,[0,_2X[1],[1,_2V[2],_2X[2]]]];}}};},_2Y=B(A(B(_2P(_2L[2])),[_2J,_2O[1]]));if(!_2Y[0]){return E(_2Y);}else{var _2Z=E(_2Y[1]);return [1,[0,_2Z[1],[1,_2O[2],_2Z[2]]]];}}}};},_30=new T(function(){return [0,function(_31,_32){var _33=B(A(B(_2H(_1o)),[_31,_32]));return _33[0]==0?[0,_33[1]]:[1,new T(function(){var _34=E(_33[1]);return [0,_34[1],[0,_34[2]]];})];}];}),_35=function(_36,_37){if(E(B(_7(function(_){var _=0;return new F(function(){return A(_e,[_36,_37,_]);});}))[1])==2){return new F(function(){return A(E(_30)[1],[_36,new T(function(){return [0,E(_37)[1]+1|0];})]);});}else{return E(_k);}},_38=[0,_35],_39=function(_3a,_3b){return [0,E(_3a)[1]+E(_3b)[1]|0];},_3c=function(_3d){var _3e=jsRound(_3d),_3f=_3e;return [0,_3f];},_3g=new T(function(){return B(_b("(function(b){return b.size;})"));}),_3h=new T(function(){return B(_b("(function(b){try {return new Blob([b]);} catch (e) {return new Blob([b.buffer]);}})"));}),_3i=new T(function(){return [0,"(function(b,off,len){return b.slice(off,len);})"];}),_3j=function(_3k,_3l,_3m){if(!E(_3k)){var _3n=B(_7(function(_){var _=0;return new F(function(){return A(_3h,[E(_3m),_]);});}));if(B(_7(function(_){var _=0,_3o=B(A(_3g,[E(_3n),_])),_3p=_3o;return new T(function(){return B(_3c(_3p));});}))[1]<=_3l){return E(_3n);}else{return new F(function(){return _7(function(_){var _=0;return new F(function(){return A(_b,[E(_3i)[1],E(_3n),E(0),E(_3l),_]);});});});}}else{return new F(function(){return _7(function(_){var _=0;return new F(function(){return A(_b,[E(_3i)[1],B(_7(function(_){var _=0;return new F(function(){return A(_3h,[E(_3m),_]);});})),E(_3k),E(_3k+_3l|0),_]);});});});}},_3q=function(_3r,_3s){var _3t=new T(function(){return [0,B(_7(function(_){var _=0;return new F(function(){return A(_K,[_3r,_3s,_]);});}))[1]];}),_3u=new T(function(){return [0,E(_3s)[1]+4|0];});return [1,[0,new T(function(){return B(_39(_3u,_3t));}),new T(function(){return B(_3j(E(_3u)[1],E(_3t)[1],_3r));})]];},_3v=[0,_3q],_3w=new T(function(){return B(unCStr("Wrong magic byte for ServerReply"));}),_3x=[0,_3w],_3y=function(_3z,_3A){if(E(B(_7(function(_){var _=0;return new F(function(){return A(_e,[_3z,_3A,_]);});}))[1])==1){var _3B=new T(function(){return [0,E(_3A)[1]+1|0];}),_3C=B(A(E(_3v)[1],[_3z,new T(function(){return [0,E(_3B)[1]+4|0];})]));if(!_3C[0]){return [0,_3C[1]];}else{var _3D=E(_3C[1]);return [1,[0,_3D[1],[0,new T(function(){return [0,B(_7(function(_){var _=0;return new F(function(){return A(_K,[_3z,_3B,_]);});}))[1]];}),_3D[2]]]];}}else{return E(_3x);}},_3E=[0,_3y],_3F=[0,0],_3G=new T(function(){return B(_b("(function(b,cb){var r=new FileReader();r.onload=function(){B(A(cb,[new DataView(r.result),0]));};r.readAsArrayBuffer(b);})"));}),_3H=[2],_3I=function(_3J){return [2];},_3K=function(_3L,_3M,_3N){return function(_){var _3O=E(_3L)[1],_3P=rMV(_3O),_3Q=_3P,_3R=E(_3Q);if(!_3R[0]){var _=wMV(_3O,[0,_3R[1],new T(function(){return B(_l(_3R[2],[1,[0,_3M,function(_3S){return E(new T(function(){return B(A(_3N,[_Y]));}));}],_C]));})]);return _3H;}else{var _3T=E(_3R[1]);if(!_3T[0]){var _=wMV(_3O,[0,_3M,_C]);return new T(function(){return B(A(_3N,[_Y]));});}else{var _=wMV(_3O,[1,_3T[2]]);return [1,[1,new T(function(){return B(A(_3N,[_Y]));}),[1,new T(function(){return B(A(_3T[1],[_3M,_3I]));}),_C]]];}}};},_3U=[1,_C],_3V=function(_3W,_3X){return function(_){var _3Y=E(_3W)[1],_3Z=rMV(_3Y),_40=_3Z,_41=E(_40);if(!_41[0]){var _42=_41[1],_43=E(_41[2]);if(!_43[0]){var _=wMV(_3Y,_3U);return new T(function(){return B(A(_3X,[_42]));});}else{var _44=E(_43[1]),_=wMV(_3Y,[0,_44[1],_43[2]]);return [1,[1,new T(function(){return B(A(_3X,[_42]));}),[1,new T(function(){return B(A(_44[2],[_3I]));}),_C]]];}}else{var _=wMV(_3Y,[1,new T(function(){return B(_l(_41[1],[1,function(_45){return function(_46){return E(new T(function(){return B(A(_3X,[_45]));}));};},_C]));})]);return _3H;}};},_47=function(_48,_){while(1){var _49=E(_48);if(!_49[0]){return _Y;}else{var _4a=_49[2],_4b=E(_49[1]);switch(_4b[0]){case 0:var _4c=B(A(_4b[1],[_])),_4d=_4c;_48=B(_l(_4a,[1,_4d,_C]));continue;case 1:_48=B(_l(_4a,_4b[1]));continue;default:_48=_4a;continue;}}}},_4e=function(_4f,_4g,_){var _4h=E(_4f);switch(_4h[0]){case 0:var _4i=B(A(_4h[1],[_])),_4j=_4i;return new F(function(){return _47(B(_l(_4g,[1,_4j,_C])),_);});break;case 1:return new F(function(){return _47(B(_l(_4g,_4h[1])),_);});break;default:return new F(function(){return _47(_4g,_);});}},_4k=function(_4l){return function(_4m){return [0,function(_){var _4n=nMV(_3U),_4o=_4n,_4p=[0,_4o];return [0,function(_){var _4q=B(A(_3G,[E(_4l),function(_4r,_){return new F(function(){return _4e(new T(function(){return [0,B(_3K(_4p,[0,_3F,new T(function(){return B(_7(function(_){var _=0,_4s=B(A(_3g,[E(_4l),_])),_4t=_4s;return new T(function(){return B(_3c(_4t));});}));}),_4r],_3I))];}),_C,_);});},_])),_4u=_4q;return new T(function(){return [0,B(_3V(_4p,_4m))];});}];}];};},_4v=new T(function(){return B(_b("(function(url, cb, f, err) {var ws = new WebSocket(url);ws.binaryType = \'blob\';ws.onmessage = function(e) {B(A(cb,[ws,e.data,0]));};ws.onopen = function(e) {B(A(f,[ws,0]));};ws.onerror = function(e) {B(A(err,[0]));};return ws;})"));}),_4w=function(_4x,_4y,_4z,_4A,_4B){return function(_){var _4C=nMV(_3U),_4D=_4C,_4E=[0,_4D],_4F=function(_4G){return [0,B(_3K(_4E,_4G,_3I))];};return [0,function(_){var _4H=B(A(_4v,[E(toJSStr(E(_4x))),function(_4I,_4J,_){return new F(function(){return _4e(new T(function(){return B(A(_4y,[_4I,_4J,_3I]));}),_C,_);});},function(_4K,_){return new F(function(){return _4e(new T(function(){return B(A(_4A,[_4K,_4F]));}),_C,_);});},function(_){return new F(function(){return _4e(new T(function(){return B(A(_4z,[_4F]));}),_C,_);});},_])),_4L=_4H;return new T(function(){return [0,B(_3V(_4E,_4B))];});}];};},_4M=function(_4N,_4O){var _4P=E(_4O);if(!_4P[0]){return [0,_C,_C];}else{var _4Q=_4P[1];if(!B(A(_4N,[_4Q]))){return [0,_C,_4P];}else{var _4R=new T(function(){var _4S=B(_4M(_4N,_4P[2]));return [0,_4S[1],_4S[2]];});return [0,[1,_4Q,new T(function(){return E(E(_4R)[1]);})],new T(function(){return E(E(_4R)[2]);})];}}},_4T=function(_4U){return new F(function(){return A(_4U,[_Y]);});},_4V=new T(function(){return B(unCStr("WebSockets connection died for some reason!"));}),_4W=new T(function(){return B(err(_4V));}),_4X=[1,_Y],_4Y=new T(function(){return B(unCStr("ServerException"));}),_4Z=new T(function(){return B(unCStr("Haste.App.Protocol"));}),_50=new T(function(){return B(unCStr("haste-lib-0.4.2.1"));}),_51=new T(function(){var _52=hs_wordToWord64(1007839920),_53=_52,_54=hs_wordToWord64(2896172365),_55=_54;return [0,_53,_55,[0,_53,_55,_50,_4Z,_4Y],_C];}),_56=function(_57){return E(_51);},_58=function(_59){return E(E(_59)[1]);},_5a=function(_5b,_5c,_5d){var _5e=B(A(_5b,[_])),_5f=B(A(_5c,[_])),_5g=hs_eqWord64(_5e[1],_5f[1]),_5h=_5g;if(!E(_5h)){return [0];}else{var _5i=hs_eqWord64(_5e[2],_5f[2]),_5j=_5i;return E(_5j)==0?[0]:[1,_5d];}},_5k=function(_5l){var _5m=E(_5l);return new F(function(){return _5a(B(_58(_5m[1])),_56,_5m[2]);});},_5n=[0,34],_5o=new T(function(){return B(unCStr("ServerException "));}),_5p=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_5q=new T(function(){return B(err(_5p));}),_5r=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_5s=new T(function(){return B(err(_5r));}),_5t=function(_5u,_5v){while(1){var _5w=E(_5u);if(!_5w[0]){return E(_5s);}else{var _5x=E(_5v);if(!_5x){return E(_5w[1]);}else{_5u=_5w[2];_5v=_5x-1|0;continue;}}}},_5y=new T(function(){return B(unCStr("ACK"));}),_5z=new T(function(){return B(unCStr("BEL"));}),_5A=new T(function(){return B(unCStr("BS"));}),_5B=new T(function(){return B(unCStr("SP"));}),_5C=[1,_5B,_C],_5D=new T(function(){return B(unCStr("US"));}),_5E=[1,_5D,_5C],_5F=new T(function(){return B(unCStr("RS"));}),_5G=[1,_5F,_5E],_5H=new T(function(){return B(unCStr("GS"));}),_5I=[1,_5H,_5G],_5J=new T(function(){return B(unCStr("FS"));}),_5K=[1,_5J,_5I],_5L=new T(function(){return B(unCStr("ESC"));}),_5M=[1,_5L,_5K],_5N=new T(function(){return B(unCStr("SUB"));}),_5O=[1,_5N,_5M],_5P=new T(function(){return B(unCStr("EM"));}),_5Q=[1,_5P,_5O],_5R=new T(function(){return B(unCStr("CAN"));}),_5S=[1,_5R,_5Q],_5T=new T(function(){return B(unCStr("ETB"));}),_5U=[1,_5T,_5S],_5V=new T(function(){return B(unCStr("SYN"));}),_5W=[1,_5V,_5U],_5X=new T(function(){return B(unCStr("NAK"));}),_5Y=[1,_5X,_5W],_5Z=new T(function(){return B(unCStr("DC4"));}),_60=[1,_5Z,_5Y],_61=new T(function(){return B(unCStr("DC3"));}),_62=[1,_61,_60],_63=new T(function(){return B(unCStr("DC2"));}),_64=[1,_63,_62],_65=new T(function(){return B(unCStr("DC1"));}),_66=[1,_65,_64],_67=new T(function(){return B(unCStr("DLE"));}),_68=[1,_67,_66],_69=new T(function(){return B(unCStr("SI"));}),_6a=[1,_69,_68],_6b=new T(function(){return B(unCStr("SO"));}),_6c=[1,_6b,_6a],_6d=new T(function(){return B(unCStr("CR"));}),_6e=[1,_6d,_6c],_6f=new T(function(){return B(unCStr("FF"));}),_6g=[1,_6f,_6e],_6h=new T(function(){return B(unCStr("VT"));}),_6i=[1,_6h,_6g],_6j=new T(function(){return B(unCStr("LF"));}),_6k=[1,_6j,_6i],_6l=new T(function(){return B(unCStr("HT"));}),_6m=[1,_6l,_6k],_6n=[1,_5A,_6m],_6o=[1,_5z,_6n],_6p=[1,_5y,_6o],_6q=new T(function(){return B(unCStr("ENQ"));}),_6r=[1,_6q,_6p],_6s=new T(function(){return B(unCStr("EOT"));}),_6t=[1,_6s,_6r],_6u=new T(function(){return B(unCStr("ETX"));}),_6v=[1,_6u,_6t],_6w=new T(function(){return B(unCStr("STX"));}),_6x=[1,_6w,_6v],_6y=new T(function(){return B(unCStr("SOH"));}),_6z=[1,_6y,_6x],_6A=new T(function(){return B(unCStr("NUL"));}),_6B=[1,_6A,_6z],_6C=[0,92],_6D=new T(function(){return B(unCStr("\\DEL"));}),_6E=new T(function(){return B(unCStr("\\a"));}),_6F=new T(function(){return B(unCStr("\\\\"));}),_6G=new T(function(){return B(unCStr("\\SO"));}),_6H=new T(function(){return B(unCStr("\\r"));}),_6I=new T(function(){return B(unCStr("\\f"));}),_6J=new T(function(){return B(unCStr("\\v"));}),_6K=new T(function(){return B(unCStr("\\n"));}),_6L=new T(function(){return B(unCStr("\\t"));}),_6M=new T(function(){return B(unCStr("\\b"));}),_6N=function(_6O,_6P){if(_6O<=127){var _6Q=E(_6O);switch(_6Q){case 92:return new F(function(){return _l(_6F,_6P);});break;case 127:return new F(function(){return _l(_6D,_6P);});break;default:if(_6Q<32){var _6R=E(_6Q);switch(_6R){case 7:return new F(function(){return _l(_6E,_6P);});break;case 8:return new F(function(){return _l(_6M,_6P);});break;case 9:return new F(function(){return _l(_6L,_6P);});break;case 10:return new F(function(){return _l(_6K,_6P);});break;case 11:return new F(function(){return _l(_6J,_6P);});break;case 12:return new F(function(){return _l(_6I,_6P);});break;case 13:return new F(function(){return _l(_6H,_6P);});break;case 14:return new F(function(){return _l(_6G,new T(function(){var _6S=E(_6P);if(!_6S[0]){var _6T=[0];}else{var _6T=E(E(_6S[1])[1])==72?B(unAppCStr("\\&",_6S)):E(_6S);}return _6T;}));});break;default:return new F(function(){return _l([1,_6C,new T(function(){var _6U=_6R;return _6U>=0?B(_5t(_6B,_6U)):E(_5q);})],_6P);});}}else{return [1,[0,_6Q],_6P];}}}else{return [1,_6C,new T(function(){var _6V=jsShowI(_6O),_6W=_6V;return B(_l(fromJSStr(_6W),new T(function(){var _6X=E(_6P);if(!_6X[0]){var _6Y=[0];}else{var _6Z=E(_6X[1])[1];if(_6Z<48){var _70=E(_6X);}else{var _70=_6Z>57?E(_6X):B(unAppCStr("\\&",_6X));}var _71=_70,_72=_71,_6Y=_72;}return _6Y;})));})];}},_73=new T(function(){return B(unCStr("\\\""));}),_74=function(_75,_76){var _77=E(_75);if(!_77[0]){return E(_76);}else{var _78=_77[2],_79=E(E(_77[1])[1]);if(_79==34){return new F(function(){return _l(_73,new T(function(){return B(_74(_78,_76));}));});}else{return new F(function(){return _6N(_79,new T(function(){return B(_74(_78,_76));}));});}}},_7a=function(_7b,_7c,_7d){return _7b<11?B(_l(_5o,[1,_5n,new T(function(){return B(_74(_7c,[1,_5n,_7d]));})])):[1,_v,new T(function(){return B(_l(_5o,[1,_5n,new T(function(){return B(_74(_7c,[1,_5n,[1,_u,_7d]]));})]));})];},_7e=function(_7f){return new F(function(){return _7a(0,E(_7f)[1],_C);});},_7g=function(_7h,_7i){return new F(function(){return _7a(0,E(_7h)[1],_7i);});},_7j=[0,44],_7k=[0,93],_7l=[0,91],_7m=function(_7n,_7o,_7p){var _7q=E(_7o);return _7q[0]==0?B(unAppCStr("[]",_7p)):[1,_7l,new T(function(){return B(A(_7n,[_7q[1],new T(function(){var _7r=function(_7s){var _7t=E(_7s);return _7t[0]==0?E([1,_7k,_7p]):[1,_7j,new T(function(){return B(A(_7n,[_7t[1],new T(function(){return B(_7r(_7t[2]));})]));})];};return B(_7r(_7q[2]));})]));})];},_7u=function(_7v,_7w){return new F(function(){return _7m(_7g,_7v,_7w);});},_7x=function(_7y,_7z,_7A){return new F(function(){return _7a(E(_7y)[1],E(_7z)[1],_7A);});},_7B=[0,_7x,_7e,_7u],_7C=new T(function(){return [0,_56,_7B,_7D,_5k];}),_7D=function(_7w){return [0,_7C,_7w];},_7E=function(_7F,_7G){return new F(function(){return die(new T(function(){return B(A(_7G,[_7F]));}));});},_7H=function(_7I){return new F(function(){return _7E(_7I,_7D);});},_7J=[0,0],_7K=function(_7L,_7M){return E(_7L)[1]!=E(_7M)[1];},_7N=function(_7O,_7P,_7Q){return [0,B(_3K(_7O,_7P,_7Q))];},_7R=new T(function(){return B(unCStr("Not enough data!"));}),_7S=[0,_7R],_7T=new T(function(){return B(_b("(function(s, msg) {s.send(msg);})"));}),_7U=function(_7V,_7W,_7X,_7Y,_){return [0,function(_){return new F(function(){return _4e([0,function(_){var _7Z=nMV(_C),_80=_7Z;return [0,function(_){var _81=nMV(_7J),_82=_81;return [0,function(_){var _83=nMV([0,function(_84,_85,_86){var _87=new T(function(){return E(E(_85)[1]);});return [0,B(_3V(_87,function(_88){return [0,B(_4w(new T(function(){return E(E(_7W)[1]);}),function(_89,_8a){return new F(function(){return (function(_8b){return function(_8c){return new F(function(){return A(new T(function(){return B(_4k(_8b));}),[function(_8d){return [0,function(_){var _8e=mMV(_80,function(_8f){if(!E(new T(function(){var _8g=E(_8d),_8h=B(A(E(_38)[1],[_8g[3],_8g[1]]));if(!_8h[0]){var _8i=E(_4X);}else{var _8j=E(_8h[1]),_8i=E(_8j[1])[1]>E(_8g[2])[1]?E(_4X):B(_7H(_8j[2]));}var _8k=_8i,_8l=_8k;return _8l;}))[0]){return [0,_8f,_4T];}else{var _8m=E(new T(function(){var _8n=E(_8d),_8o=B(A(E(_3E)[1],[_8n[3],_8n[1]]));if(!_8o[0]){var _8p=[0,_8o[1]];}else{var _8q=E(_8o[1]),_8p=E(_8q[1])[1]>E(_8n[2])[1]?E(_7S):[1,_8q[2]];}var _8r=_8p,_8s=_8r;return _8s;}));if(!_8m[0]){return [0,_8f,_4T];}else{var _8t=E(_8m[1]),_8u=B(_4M(function(_8v){return new F(function(){return _7K(E(_8v)[1],_8t[1]);});},_8f)),_8w=E(_8u[2]);return _8w[0]==0?[0,_8f,_4T]:[0,new T(function(){return B(_l(_8u[1],_8w[2]));}),function(_8x){return new F(function(){return _7N(E(_8w[1])[2],_8t[2],_8x);});}];}}}),_8y=_8e;return new T(function(){return B(A(_8y,[_8c]));});}];}]);});};})(_8a);});},_4W,_0,function(_8z){return [0,B(_3K(_87,function(_8A){return function(_8B,_8C){return [0,function(_){var _8D=B(A(new T(function(){return B(A(_7T,[E(_8z),E(_8A)]));}),[_])),_8E=_8D;return new T(function(){return B(A(_8C,[_Y]));});}];};},function(_8F){return E([0,function(_){var _8G=B(A(_7T,[E(_8z),E(_84),_])),_8H=_8G;return new T(function(){return B(A(_86,[_Y]));});}]);}))];}))];}))];},_C]),_8I=_83;return new T(function(){return B(A(_7V,[[0,[0,_8I],[0,_82],[0,_80]],_3I]));});}];}];}],_C,_);});},_7X,_7Y,_7W];},_8J=[0,1],_8K=[0,24601],_8L=new T(function(){return B(unCStr("ws://localhost:24601"));}),_8M=[0,_8L,_8K,_C],_8N=[0,0],_8O=[0,_8N,_C],_8P=new T(function(){return B(unCStr("requests"));}),_8Q=[0,0],_8R=function(_8S){return [0,B(_2H(_8S))];},_8T=function(_8U,_){return _Y;},_8V=function(_8W,_8X){while(1){var _8Y=E(_8W);if(!_8Y[0]){return E(_8X);}else{_8W=_8Y[2];var _8Z=_8X+1|0;_8X=_8Z;continue;}}},_90=new T(function(){return [0,"Uint32Array"];}),_91=function(_92){return function(_93,_){var _94=B(A(_b,[E(_Z)[1],E(_93),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_90,_16,new T(function(){return E(E(_92)[1]);}),_]);});}));})),_])),_95=_94;return _Y;};},_96=function(_97){return E(E(_97)[2]);},_98=function(_99,_9a){return function(_9b,_){var _9c=B(A(B(_91(new T(function(){return [0,B(_8V(_9a,0))>>>0];}))),[_9b,_])),_9d=_9c;return new F(function(){return A(E(new T(function(){var _9e=function(_9f){var _9g=E(_9f);return _9g[0]==0?_8T:function(_9h,_){var _9i=B(A(B(A(new T(function(){return B(_96(_99));}),[_9g[1]]))[1],[_9h,_])),_9j=_9i;return new F(function(){return A(E(new T(function(){return [0,B(_9e(_9g[2]))];}))[1],[_9h,_]);});};};return [0,B(_9e(_9a))];}))[1],[_9b,_]);});};},_9k=function(_9l,_9m){return [0,B(_98(_9l,_9m))];},_9n=function(_9o){return [0,new T(function(){return B(_8R(_9o));}),function(_9p){return new F(function(){return _9k(_9o,_9p);});}];},_9q=new T(function(){return B(_9n(_1o));}),_9r=function(_9s,_9t,_9u){return [0,function(_){var _9v=B(A(_9s,[_])),_9w=_9v;return new T(function(){return B(A(_9u,[_9w]));});}];},_9x=function(_9y,_9z){var _9A=strEq(E(_9y)[1],E(_9z)[1]),_9B=_9A;return E(_9B)==0?true:false;},_9C=function(_9D,_9E){var _9F=strEq(E(_9D)[1],E(_9E)[1]),_9G=_9F;return E(_9G)==0?false:true;},_9H=[0,_9C,_9x],_9I=new T(function(){return [0,"action"];}),_9J=new T(function(){return [0,"controller"];}),_9K=new T(function(){return [0,"path"];}),_9L=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_9M=[0,_9L],_9N=new T(function(){return B(unCStr("Key not found"));}),_9O=[0,_9N],_9P=new T(function(){return [0,"verb"];}),_9Q=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_9R=[0,_9Q],_9S=function(_9T){return new F(function(){return fromJSStr(E(_9T)[1]);});},_9U=function(_9V){return E(E(_9V)[1]);},_9W=function(_9X,_9Y,_9Z){while(1){var _a0=E(_9Z);if(!_a0[0]){return [0];}else{var _a1=E(_a0[1]);if(!B(A(_9U,[_9X,_9Y,_a1[1]]))){_9Z=_a0[2];continue;}else{return [1,_a1[2]];}}}},_a2=function(_a3){var _a4=E(_a3);if(_a4[0]==4){var _a5=_a4[1],_a6=B(_9W(_9H,_9P,_a5));if(!_a6[0]){return E(_9O);}else{var _a7=E(_a6[1]);if(_a7[0]==1){var _a8=B(_9W(_9H,_9K,_a5));if(!_a8[0]){return E(_9O);}else{var _a9=E(_a8[1]);if(_a9[0]==1){var _aa=B(_9W(_9H,_9J,_a5));if(!_aa[0]){return E(_9O);}else{var _ab=E(_aa[1]);if(_ab[0]==1){var _ac=B(_9W(_9H,_9I,_a5));if(!_ac[0]){return E(_9O);}else{var _ad=E(_ac[1]);return _ad[0]==1?[1,[0,new T(function(){return fromJSStr(E(_a7[1])[1]);}),new T(function(){return B(_9S(_a9[1]));}),new T(function(){return B(_9S(_ab[1]));}),new T(function(){return B(_9S(_ad[1]));})]]:E(_9M);}}else{return E(_9M);}}}else{return E(_9M);}}}else{return E(_9M);}}}else{return E(_9R);}},_ae=true,_af=function(_ag,_ah){return new F(function(){return A(_ah,[_Y]);});},_ai=new T(function(){return B(_b("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_aj=function(_){var _=0;return new F(function(){return A(_b,["false",_]);});},_ak=new T(function(){return B(_7(_aj));}),_al=function(_){var _=0;return new F(function(){return A(_b,["true",_]);});},_am=new T(function(){return B(_7(_al));}),_an=function(_ao){return function(_ap){return function(_aq,_){var _ar=B(A(new T(function(){return B(A(new T(function(){return B(A(_ai,[E(E(_ao)[1])]));}),[E(toJSStr(E(_ap)))]));}),[!E(_aq)?E(_ak):E(_am),_])),_as=_ar;return _Y;};};},_at=function(_au,_av,_aw,_ax,_ay,_az){return function(_){var _aA=jsCreateElem(toJSStr(E(_au))),_aB=_aA;return [0,function(_){var _aC=jsCreateTextNode(toJSStr(E(_av))),_aD=_aC;return [0,function(_){var _aE=jsAppendChild(_aD,_aB);return new T(function(){var _aF=function(_aG){var _aH=E(_aG);return _aH[0]==0?E(_af):function(_aI,_aJ){return [0,function(_){var _aK=B(A(new T(function(){return B(A(new T(function(){return B(_an([0,_aB]));}),[_aH[1],_ae]));}),[_])),_aL=_aK;return new T(function(){return B(A(new T(function(){return B(_aF(_aH[2]));}),[_aI,_aJ]));});}];};};return B(A(_aF,[_aw,_ay,function(_aM){return E([0,function(_){var _aN=jsAppendChild(_aB,E(_ax)[1]);return new T(function(){return B(A(_az,[_Y]));});}]);}]));});}];}];};},_aO=function(_aP){return [0,function(_aQ,_){var _aR=B(A(B(_1h(new T(function(){return [0,B(_7(function(_){var _=0,_aS=B(A(_3g,[E(_aP),_])),_aT=_aS;return new T(function(){return B(_3c(_aT));});}))[1]];}))),[_aQ,_])),_aU=_aR,_aV=B(A(_b,[E(_Z)[1],E(_aQ),E(_aP),_])),_aW=_aV;return _Y;}];},_aX=[0,_3v,_aO],_aY=[0,0],_aZ=[0,1],_b0=new T(function(){return [0,"Uint8Array"];}),_b1=function(_b2){return function(_b3,_){var _b4=B(A(_b,[E(_Z)[1],E(_b3),E(new T(function(){return B(_7(function(_){var _=0;return new F(function(){return A(_1c,[_15,_b0,_aZ,new T(function(){return E(E(_b2)[1]);}),_]);});}));})),_])),_b5=_b4;return _Y;};},_b6=new T(function(){return [0,B(_b1(_aY))];}),_b7=function(_b8,_b9,_ba){return function(_bb,_){var _bc=B(A(E(_b6)[1],[_bb,_])),_bd=_bc,_be=B(A(E(new T(function(){return [0,B(_1h(new T(function(){return [0,E(_b8)[1]];})))];}))[1],[_bb,_])),_bf=_be,_bg=B(A(E(new T(function(){return [0,B(_1h(new T(function(){return [0,E(_b9)[1]];})))];}))[1],[_bb,_])),_bh=_bg;return new F(function(){return A(E(new T(function(){return [0,B(_98(_aX,_ba))];}))[1],[_bb,_]);});};},_bi=new T(function(){return B(unCStr("(function(){return [];})"));}),_bj=new T(function(){return B(_b("(function(parts){return new Blob(parts);})"));}),_bk=function(_bl){return new F(function(){return _7(function(_){var _=0,_bm=B(A(_b,[toJSStr(E(_bi)),_])),_bn=_bm,_bo=B(A(_bl,[_bn,_])),_bp=_bo;return new F(function(){return A(_bj,[E(_bn),_]);});});});},_bq=function(_br){return [0,new T(function(){return [0,E(_br)[1]+1|0];}),_br];},_bs=new T(function(){return B(unCStr("Unable to decode return value!"));}),_bt=new T(function(){return B(err(_bs));}),_bu=function(_bv,_bw){while(1){var _bx=E(_bv);if(!_bx[0]){return E(_bw);}else{_bv=_bx[2];var _by=[1,_bx[1],_bw];_bw=_by;continue;}}},_bz=function(_bA,_bB,_bC){return function(_bD,_bE){var _bF=new T(function(){return E(E(_bD)[1]);});return [0,B(_3V(_bF,function(_bG){return [0,B(_3K(_bF,_bG,function(_bH){return E([0,function(_){var _bI=nMV(_3U),_bJ=_bI,_bK=[0,_bJ];return [0,function(_){var _bL=E(_bD),_bM=mMV(E(_bL[2])[1],_bq),_bN=_bM;return [0,function(_){var _bO=mMV(E(_bL[3])[1],function(_bP){return [0,[1,[0,_bN,_bK],_bP],_Y];}),_bQ=_bO;return new T(function(){return B(A(_bG,[new T(function(){return B(_bk(B(_b7(_bN,_bB,new T(function(){return B(_bu(_bC,_C));})))));}),_bL,function(_bR){return [0,B(_3V(_bK,function(_bS){return new F(function(){return A(_4k,[_bS,function(_bT){var _bU=E(_bT),_bV=B(A(E(new T(function(){return B(_2v(_bA));}))[1],[_bU[3],_bU[1]]));if(!_bV[0]){return E(_bt);}else{var _bW=E(_bV[1]);return E(_bW[1])[1]>E(_bU[2])[1]?E(_bt):B(A(_bE,[_bW[2]]));}}]);});}))];}]));});}];}];}]);}))];}))];};},_bX=function(_bY,_bZ,_c0,_c1){return new F(function(){return A(_bY,[function(_){var _c2=jsSetAttr(E(_bZ)[1],toJSStr(E(_c0)),toJSStr(E(_c1)));return _Y;}]);});},_c3=new T(function(){return B(unCStr("Invalid JSON!"));}),_c4=function(_c5,_c6){while(1){var _c7=E(_c5);if(!_c7[0]){return E(_c6)[0]==0?true:false;}else{var _c8=E(_c6);if(!_c8[0]){return false;}else{if(E(_c7[1])[1]!=E(_c8[1])[1]){return false;}else{_c5=_c7[2];_c6=_c8[2];continue;}}}}},_c9=new T(function(){return B(unCStr("ac-small"));}),_ca=new T(function(){return B(unCStr("request"));}),_cb=new T(function(){return B(unCStr("span"));}),_cc=new T(function(){return B(unCStr("delete"));}),_cd=new T(function(){return B(unCStr("get"));}),_ce=new T(function(){return B(unCStr("post"));}),_cf=new T(function(){return B(unCStr("put"));}),_cg=new T(function(){return B(unCStr("unexpected_verb"));}),_ch=new T(function(){return B(unCStr("PUT"));}),_ci=new T(function(){return B(unCStr("POST"));}),_cj=new T(function(){return B(unCStr("GET"));}),_ck=new T(function(){return B(unCStr("DELETE"));}),_cl=new T(function(){return B(unCStr("label"));}),_cm=new T(function(){return B(unCStr("for"));}),_cn=new T(function(){return B(unCStr("checkbox"));}),_co=new T(function(){return B(unCStr("type"));}),_cp=new T(function(){return B(unCStr("accordion-1"));}),_cq=new T(function(){return B(unCStr("name"));}),_cr=new T(function(){return B(unCStr("id"));}),_cs=new T(function(){return B(unCStr("list-group-item"));}),_ct=[0,35],_cu=[1,_ct,_C],_cv=new T(function(){return B(unCStr("href"));}),_cw=new T(function(){return B(unCStr("alert-danger"));}),_cx=new T(function(){return B(unCStr("alert"));}),_cy=new T(function(){return B(unCStr("article"));}),_cz=new T(function(){return B(unCStr("input"));}),_cA=[0,97],_cB=[1,_cA,_C],_cC=new T(function(){return B(unCStr("div"));}),_cD=new T(function(){return B(unCStr("small"));}),_cE=[1,_cD,_C],_cF=new T(function(){return B(unCStr("text-muted"));}),_cG=[1,_cF,_cE],_cH=new T(function(){return B(unCStr("pull-right"));}),_cI=[1,_cH,_cG],_cJ=new T(function(){return B(unCStr("path"));}),_cK=[1,_cJ,_cI],_cL=function(_cM,_cN,_cO){var _cP=new T(function(){return B(unAppCStr("ac-",new T(function(){return B(_w(0,E(_cM)[1],_C));})));}),_cQ=new T(function(){return B(_cL(new T(function(){return [0,E(_cM)[1]+1|0];}),_cN,_cO));}),_cR=function(_cS,_cT,_cU){return [0,function(_){var _cV=jsCreateElem(toJSStr(E(_cC))),_cW=_cV,_cX=[0,_cW];return [0,function(_){var _cY=jsCreateTextNode(toJSStr(E(_cS))),_cZ=_cY;return [0,function(_){var _d0=jsAppendChild(_cZ,_cW);return [0,function(_){var _d1=B(A(_an,[_cX,_cx,_ae,_])),_d2=_d1;return [0,function(_){var _d3=B(A(_an,[_cX,_cw,_ae,_])),_d4=_d3;return [0,function(_){var _d5=jsAppendChild(_cW,E(_cO)[1]);return new T(function(){return B(A(_cQ,[_cT,_cU]));});}];}];}];}];}];}];};return function(_d6){return function(_d7){return new F(function(){return A(new T(function(){return B(A(new T(function(){var _d8=E(_cN);return B(_bz(_9q,_d8[1],_d8[2]));}),[_d6]));}),[function(_d9){var _da=jsParseJSON(toJSStr(E(_d9))),_db=_da,_dc=E(_db);if(!_dc[0]){return E(new T(function(){return B(_cR(_c3,_d6,_d7));}));}else{var _dd=B(_a2(_dc[1]));if(!_dd[0]){return new F(function(){return _cR(_dd[1],_d6,_d7);});}else{var _de=_dd[1];return [0,function(_){var _df=jsCreateElem(toJSStr(_cB)),_dg=_df;return new T(function(){var _dh=[0,_dg];return B(A(_bX,[_9r,_dh,_cv,_cu,_d6,function(_di){return E([0,function(_){var _dj=B(A(_an,[_dh,_cs,_ae,_])),_dk=_dj;return [0,function(_){var _dl=jsCreateElem(toJSStr(E(_cz))),_dm=_dl;return new T(function(){var _dn=[0,_dm];return B(A(_bX,[_9r,_dn,_cr,_cP,_d6,function(_do){return E(new T(function(){return B(A(_bX,[_9r,_dn,_cq,_cp,_d6,function(_dp){return E(new T(function(){return B(A(_bX,[_9r,_dn,_co,_cn,_d6,function(_dq){return E([0,function(_){var _dr=jsAppendChild(_dm,_dg);return [0,function(_){var _ds=E(_cl),_dt=jsCreateElem(toJSStr(_ds)),_du=_dt;return new T(function(){return B(A(_bX,[_9r,[0,_du],_cm,_cP,_d6,function(_dv){return E([0,function(_){var _dw=E(_cb),_dx=jsCreateElem(toJSStr(_dw)),_dy=_dx;return new T(function(){var _dz=[0,_dy];return [0,B(_at(_dw,new T(function(){return E(E(_de)[1]);}),[1,_ds,[1,new T(function(){var _dA=E(_de)[1];return !B(_c4(_dA,_ck))?!B(_c4(_dA,_cj))?!B(_c4(_dA,_ci))?!B(_c4(_dA,_ch))?E(_cg):E(_cf):E(_ce):E(_cd):E(_cc);}),_C]],_dz,_d6,function(_dB){return [0,B(_at(_dw,new T(function(){return E(E(_de)[2]);}),_cK,_dz,_d6,function(_dC){return E([0,function(_){var _dD=B(A(_an,[_dz,_ca,_ae,_])),_dE=_dD;return [0,function(_){var _dF=jsAppendChild(_dy,_du);return [0,function(_){var _dG=jsAppendChild(_du,_dg);return [0,function(_){var _dH=jsCreateElem(toJSStr(E(_cy))),_dI=_dH;return [0,function(_){var _dJ=B(A(_an,[[0,_dI],_c9,_ae,_])),_dK=_dJ;return [0,function(_){var _dL=E(_de),_dM=jsCreateTextNode(toJSStr(B(_l(_dL[3],[1,_ct,_dL[4]])))),_dN=_dM;return [0,function(_){var _dO=jsAppendChild(_dN,_dI);return [0,function(_){var _dP=jsAppendChild(_dI,_dg);return [0,function(_){var _dQ=jsAppendChild(_dg,E(_cO)[1]);return new T(function(){return B(A(_cQ,[_d6,_d7]));});}];}];}];}];}];}];}];}];}]);}))];}))];});}]);}]));});}];}]);}]));}));}]));}));}]));});}];}]);}]));});}];}}}]);});};};},_dR=new T(function(){return B(unCStr(" could be found!"));}),_dS=function(_dT){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_l(_dT,_dR));}))));});},_dU=function(_dV,_dW){return [0,function(_){var _dX=E(_8P),_dY=jsFind(toJSStr(_dX)),_dZ=_dY;return new T(function(){var _e0=E(_dZ);return _e0[0]==0?B(_dS(_dX)):B(A(_cL,[_8Q,_8O,_e0[1],_dV,_dW]));});}];},_e1=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_e2=new T(function(){return B(unCStr("base"));}),_e3=new T(function(){return B(unCStr("AsyncException"));}),_e4=new T(function(){var _e5=hs_wordToWord64(2363394409),_e6=_e5,_e7=hs_wordToWord64(2156861182),_e8=_e7;return [0,_e6,_e8,[0,_e6,_e8,_e2,_e1,_e3],_C];}),_e9=new T(function(){return B(unCStr("BlockedIndefinitelyOnMVar"));}),_ea=new T(function(){var _eb=hs_wordToWord64(3902241243),_ec=_eb,_ed=hs_wordToWord64(2363891371),_ee=_ed;return [0,_ec,_ee,[0,_ec,_ee,_e2,_e1,_e9],_C];}),_ef=new T(function(){return B(unCStr("BlockedIndefinitelyOnSTM"));}),_eg=new T(function(){var _eh=hs_wordToWord64(2085292455),_ei=_eh,_ej=hs_wordToWord64(2226117263),_ek=_ej;return [0,_ei,_ek,[0,_ei,_ek,_e2,_e1,_ef],_C];}),_el=function(_){return _Y;},_em=new T(function(){return B(unCStr("Deadlock"));}),_en=new T(function(){var _eo=hs_wordToWord64(51525854),_ep=_eo,_eq=hs_wordToWord64(2498035378),_er=_eq;return [0,_ep,_er,[0,_ep,_er,_e2,_e1,_em],_C];}),_es=new T(function(){return B(unCStr("GHC.Exception"));}),_et=new T(function(){return B(unCStr("base"));}),_eu=new T(function(){return B(unCStr("ErrorCall"));}),_ev=new T(function(){var _ew=hs_wordToWord64(1788961336),_ex=_ew,_ey=hs_wordToWord64(3513572579),_ez=_ey;return [0,_ex,_ez,[0,_ex,_ez,_et,_es,_eu],_C];}),_eA=[0,0],_eB=function(_eC){return E(E(_eC)[2]);},_eD=function(_){var _=0,_eE=jsMkStdout(),_eF=_eE;return [0,_eF];},_eG=new T(function(){return B(_7(_eD));}),_eH=function(_){var _eI=jsFlushHandle(E(_eG)[1]);return _Y;},_eJ=function(_eK,_eL,_){var _=writeOffAddr("w32",4,E(_eK)[1],0,E(_eL)[1]);return _Y;},_eM=function(_eN,_){var _eO=readOffAddr("w32",4,E(_eN)[1],0),_eP=_eO;return [0,_eP];},_eQ=function(_eR,_eS,_eT,_){var _=writeOffAddr("w32",4,plusAddr(E(_eR)[1],E(_eS)[1]),0,E(_eT)[1]);return _Y;},_eU=function(_eV,_eW,_){var _eX=readOffAddr("w32",4,plusAddr(E(_eV)[1],E(_eW)[1]),0),_eY=_eX;return [0,_eY];},_eZ=[0,4],_f0=function(_f1){return E(_eZ);},_f2=function(_f3,_f4,_){var _f5=readOffAddr("w32",4,E(_f3)[1],E(_f4)[1]),_f6=_f5;return [0,_f6];},_f7=function(_f8,_f9,_fa,_){var _=writeOffAddr("w32",4,E(_f8)[1],E(_f9)[1],E(_fa)[1]);return _Y;},_fb=[0,_f0,_f0,_f2,_f7,_eU,_eQ,_eM,_eJ],_fc=[0],_fd=1,_fe=0,_ff=[0,_fe,_C],_fg=function(_fh,_fi,_fj,_fk,_fl,_fm,_){var _fn=nMV(_ff),_fo=_fn;return new F(function(){return (function(_fp,_fq,_){while(1){var _fr=(function(_fs,_ft,_){var _fu=E(_fh),_fv=B(A(_fu[1],[_fs,_ft,_])),_fw=_fv,_fx=E(_fw),_fy=_fx[3],_fz=E(_fx[2]);if(_fz[5]!=_fz[6]){if(E(_fx[1])==1){return _fc;}else{var _fA=B(A(_fu[2],[_fz,_fy,_])),_fB=_fA,_fC=E(_fB);_fp=_fC[1];_fq=_fC[2];return null;}}else{var _fD=function(_fE){var _fF=E(_fy),_fG=_fF[1],_fH=_fF[2],_fI=_fF[5],_fJ=_fF[6];if(!E(_fi)){var _fK=B(A(_fm,[[0,[0,_fG],[0,_fJ-_fI|0]],_])),_fL=_fK,_=0;return [1,_fL];}else{var _=writeOffAddr("w8",1,_fG,_fJ,0),_fM=B(A(_fm,[[0,[0,_fG],[0,_fJ-_fI|0]],_])),_fN=_fM,_=0;return [1,_fN];}};if(!E(_fi)){return new F(function(){return _fD(_);});}else{var _fO=E(_fy);return (_fO[4]-_fO[6]|0)==0?_fc:B(_fD(_));}}})(_fp,_fq,_);if(_fr!=null){return _fr;}}})(_fj,new T(function(){return [0,_fk,[0,_fo],_fd,E(_fl)[1],0,0];}),_);});},_fP=0,_fQ=function(_fR,_fS,_fT,_){var _fU=0,_fV=_fU;switch(E(_fV)){case 0:return new F(function(){return (function(_){var _fW=B(A(_fR,[_])),_fX=_fW,_fY=jsCatch(function(_){return new F(function(){return new T(function(){return B(A(_fT,[_fX]));})();});},function(_fZ,_){var _g0=B(A(_fS,[_fX,_])),_g1=_g0;return new F(function(){return die(_fZ);});}),_g2=_fY,_g3=B(A(_fS,[_fX,_])),_g4=_g3;return _g2;})();});break;case 1:var _g5=B(A(_fR,[_])),_g6=_g5,_g7=jsCatch(new T(function(){return B(A(_fT,[_g6]));}),function(_g8,_){var _g9=B(A(_fS,[_g6,_])),_ga=_g9;return new F(function(){return die(_g8);});}),_gb=_g7,_gc=B(A(_fS,[_g6,_])),_gd=_gc;return _gb;default:var _ge=B(A(_fR,[_])),_gf=_ge,_gg=jsCatch(new T(function(){return B(A(_fT,[_gf]));}),function(_gh,_){var _gi=B(A(_fS,[_gf,_])),_gj=_gi;return new F(function(){return die(_gh);});}),_gk=_gg,_gl=B(A(_fS,[_gf,_])),_gm=_gl;return _gk;}},_gn=function(_go){return E(E(_go)[3]);},_gp=function(_gq){return E(E(_gq)[4]);},_gr=function(_gs,_gt,_gu,_){return new F(function(){return (function(_gv,_gw,_){while(1){var _gx=E(_gv);if(!_gx[0]){return _Y;}else{var _gy=B(A(new T(function(){return B(_gp(_gs));}),[_gt,[0,_gw],_gx[1],_])),_gz=_gy;_gv=_gx[2];var _gA=_gw+1|0;_gw=_gA;continue;}}})(_gu,0,_);});},_gB=function(_gC,_gD,_gE,_){return new F(function(){return _fQ(E(_gC)[3],_gn,function(_gF,_){var _gG=B(_8V(_gD,0)),_gH=newByteArr(imul(_gG,4)|0),_gI=_gH,_gJ=_gI,_gK=_gJ,_gL=_gK,_gM=B(_gr(_fb,[0,_gL],_gD,_)),_gN=_gM,_gO=nMV(_ff),_gP=_gO,_gQ=function(_gR,_){var _gS=newByteArr(_gR),_gT=_gS,_gU=_gT,_gV=_gU,_gW=B(_fg(_gF,_ae,[0,_gL,[0,_gP],_fP,_gG,0,_gG],_gV,[0,_gR],function(_gX){return new F(function(){return A(_gE,[E(_gX)[1]]);});},_)),_gY=_gW,_gZ=E(_gY);if(!_gZ[0]){var _h0=B(_gQ(imul(_gR,2)|0,_)),_h1=_h0,_=0;return _h1;}else{var _=0;return _gZ[1];}},_h2=B(_gQ(_gG+1|0,_)),_h3=_h2,_=0;return _h3;},_);});},_h4=1,_h5=new T(function(){return B(unCStr("UTF16LE"));}),_h6=new T(function(){return B(unCStr("UTF16BE"));}),_h7=new T(function(){return B(unCStr("UTF16"));}),_h8=new T(function(){return B(unCStr("UTF8"));}),_h9=new T(function(){return B(unCStr("UTF32LE"));}),_ha=new T(function(){return B(unCStr("UTF32BE"));}),_hb=new T(function(){return B(unCStr("UTF32"));}),_hc=function(_hd){var _he=u_towupper(_hd),_hf=_he;return _hf>>>0>1114111?B(_D(_hf)):_hf;},_hg=function(_hh){while(1){var _hi=(function(_hj){var _hk=E(_hj);if(!_hk[0]){return [0];}else{var _hl=_hk[2],_hm=E(E(_hk[1])[1]);if(_hm==45){_hh=_hl;return null;}else{return [1,new T(function(){return [0,B(_hc(_hm))];}),new T(function(){return B(_hg(_hl));})];}}})(_hh);if(_hi!=null){return _hi;}}},_hn=new T(function(){return B(unCStr("UTF-32LE"));}),_ho=[0,0],_hp=function(_hq){return E(E(_hq)[3]);},_hr=function(_hs,_ht,_hu,_){if(_ht>0){return new F(function(){return (function(_hv,_hw,_){while(1){var _hx=E(_hv);if(!_hx){var _hy=B(A(new T(function(){return B(A(_hp,[_hs,_hu,_ho]));}),[_])),_hz=_hy;return [1,_hz,_hw];}else{var _hA=B(A(new T(function(){return B(_hp(_hs));}),[_hu,[0,_hx],_])),_hB=_hA;_hv=_hx-1|0;var _hC=[1,_hB,_hw];_hw=_hC;continue;}}})(_ht-1|0,_C,_);});}else{return _C;}},_hD=new T(function(){return B(unCStr("mallocForeignPtrBytes: size must be >= 0"));}),_hE=new T(function(){return B(err(_hD));}),_hF=function(_hG,_hH,_){var _hI=B((function(_hJ,_){while(1){var _hK=readOffAddr("i8",1,_hH,_hJ),_hL=_hK;if(!E(_hL)){return [0,_hJ];}else{var _hM=_hJ+1|0;_hJ=_hM;continue;}}})(0,_)),_hN=_hI;return new F(function(){return _fQ(E(_hG)[2],_gn,function(_hO,_){var _hP=nMV(_ff),_hQ=_hP,_hR=E(_hN)[1],_hS=function(_hT){var _hU=imul(_hT,4)|0;if(_hU>=0){var _hV=nMV(_ff),_hW=_hV,_hX=newByteArr(_hU),_hY=_hX,_hZ=function(_i0,_){var _i1=E(_hO),_i2=B(A(_i1[1],[_i0,[0,_hY,[1,_hY,_hW],_fd,_hT,0,0],_])),_i3=_i2,_i4=E(_i3),_i5=_i4[3],_i6=E(_i4[2]);if(_i6[5]!=_i6[6]){if(E(_i4[1])==1){var _i7=E(_i5),_i8=_i7[2],_i9=B(_hr(_fb,_i7[6]-_i7[5]|0,[0,_i7[1]],_)),_ia=_i9,_=0,_ib=B(_hZ(_i6,_)),_ic=_ib;return new T(function(){return B(_l(_ia,_ic));});}else{var _id=B(A(_i1[2],[_i6,_i5,_])),_ie=_id,_if=E(_ie),_ig=E(_if[2]),_ih=_ig[2],_ii=B(_hr(_fb,_ig[6]-_ig[5]|0,[0,_ig[1]],_)),_ij=_ii,_=0,_ik=B(_hZ(_if[1],_)),_il=_ik;return new T(function(){return B(_l(_ij,_il));});}}else{var _im=E(_i5),_in=_im[2],_io=B(_hr(_fb,_im[6]-_im[5]|0,[0,_im[1]],_)),_ip=_io,_=0;return _ip;}};return new F(function(){return _hZ([0,_hH,[0,_hQ],_fP,_hR,0,_hR],_);});}else{return E(_hE);}};return _hR>1?B(_hS(_hR)):B(_hS(1));},_);});},_iq=function(_ir,_is,_it,_iu){return new F(function(){return _7(function(_){var _=0,_iv=strerror(_is),_iw=_iv,_ix=B(A(E(_iy)[1],[_])),_iz=_ix,_iA=B(_hF(_iz,_iw,_)),_iB=_iA;return [0,_it,new T(function(){switch(E(_is)){case 1:var _iC=6;break;case 2:var _iC=1;break;case 3:var _iC=1;break;case 4:var _iC=18;break;case 5:var _iC=14;break;case 6:var _iC=1;break;case 7:var _iC=3;break;case 8:var _iC=12;break;case 9:var _iC=12;break;case 10:var _iC=1;break;case 11:var _iC=3;break;case 12:var _iC=3;break;case 13:var _iC=6;break;case 15:var _iC=12;break;case 16:var _iC=2;break;case 17:var _iC=0;break;case 18:var _iC=15;break;case 19:var _iC=15;break;case 20:var _iC=13;break;case 21:var _iC=13;break;case 22:var _iC=12;break;case 23:var _iC=3;break;case 24:var _iC=3;break;case 25:var _iC=5;break;case 26:var _iC=2;break;case 27:var _iC=6;break;case 28:var _iC=3;break;case 29:var _iC=15;break;case 30:var _iC=6;break;case 31:var _iC=3;break;case 32:var _iC=17;break;case 33:var _iC=12;break;case 34:var _iC=15;break;case 35:var _iC=2;break;case 36:var _iC=12;break;case 37:var _iC=3;break;case 38:var _iC=15;break;case 39:var _iC=8;break;case 40:var _iC=12;break;case 42:var _iC=1;break;case 43:var _iC=17;break;case 60:var _iC=12;break;case 61:var _iC=1;break;case 62:var _iC=16;break;case 63:var _iC=3;break;case 64:var _iC=1;break;case 66:var _iC=5;break;case 67:var _iC=17;break;case 69:var _iC=8;break;case 70:var _iC=17;break;case 71:var _iC=10;break;case 72:var _iC=15;break;case 74:var _iC=13;break;case 78:var _iC=17;break;case 84:var _iC=12;break;case 87:var _iC=3;break;case 88:var _iC=12;break;case 89:var _iC=12;break;case 90:var _iC=3;break;case 91:var _iC=10;break;case 92:var _iC=15;break;case 93:var _iC=10;break;case 94:var _iC=15;break;case 95:var _iC=15;break;case 96:var _iC=15;break;case 97:var _iC=15;break;case 98:var _iC=2;break;case 99:var _iC=15;break;case 100:var _iC=17;break;case 101:var _iC=1;break;case 102:var _iC=17;break;case 104:var _iC=17;break;case 105:var _iC=3;break;case 106:var _iC=0;break;case 107:var _iC=12;break;case 108:var _iC=5;break;case 109:var _iC=3;break;case 110:var _iC=16;break;case 111:var _iC=1;break;case 112:var _iC=1;break;case 113:var _iC=1;break;case 114:var _iC=0;break;case 115:var _iC=0;break;case 116:var _iC=17;break;case 122:var _iC=6;break;default:var _iC=11;}return _iC;}),_ir,_iB,[1,[0,_is]],_iu];});});},_iD=0,_iE=1,_iF=new T(function(){return [0, -(1&4294967295)>>>0];}),_iG=[0,0],_iH=new T(function(){return B(unCStr("IOException"));}),_iI=new T(function(){var _iJ=hs_wordToWord64(4053623282),_iK=_iJ,_iL=hs_wordToWord64(3693590983),_iM=_iL;return [0,_iK,_iM,[0,_iK,_iM,_e2,_e1,_iH],_C];}),_iN=function(_iO){return E(_iI);},_iP=function(_iQ){var _iR=E(_iQ);return new F(function(){return _5a(B(_58(_iR[1])),_iN,_iR[2]);});},_iS=new T(function(){return B(unCStr(": "));}),_iT=[0,41],_iU=new T(function(){return B(unCStr(" ("));}),_iV=new T(function(){return B(unCStr("already exists"));}),_iW=new T(function(){return B(unCStr("does not exist"));}),_iX=new T(function(){return B(unCStr("protocol error"));}),_iY=new T(function(){return B(unCStr("failed"));}),_iZ=new T(function(){return B(unCStr("invalid argument"));}),_j0=new T(function(){return B(unCStr("inappropriate type"));}),_j1=new T(function(){return B(unCStr("hardware fault"));}),_j2=new T(function(){return B(unCStr("unsupported operation"));}),_j3=new T(function(){return B(unCStr("timeout"));}),_j4=new T(function(){return B(unCStr("resource vanished"));}),_j5=new T(function(){return B(unCStr("interrupted"));}),_j6=new T(function(){return B(unCStr("resource busy"));}),_j7=new T(function(){return B(unCStr("resource exhausted"));}),_j8=new T(function(){return B(unCStr("end of file"));}),_j9=new T(function(){return B(unCStr("illegal operation"));}),_ja=new T(function(){return B(unCStr("permission denied"));}),_jb=new T(function(){return B(unCStr("user error"));}),_jc=new T(function(){return B(unCStr("unsatisified constraints"));}),_jd=new T(function(){return B(unCStr("system error"));}),_je=function(_jf,_jg){switch(E(_jf)){case 0:return new F(function(){return _l(_iV,_jg);});break;case 1:return new F(function(){return _l(_iW,_jg);});break;case 2:return new F(function(){return _l(_j6,_jg);});break;case 3:return new F(function(){return _l(_j7,_jg);});break;case 4:return new F(function(){return _l(_j8,_jg);});break;case 5:return new F(function(){return _l(_j9,_jg);});break;case 6:return new F(function(){return _l(_ja,_jg);});break;case 7:return new F(function(){return _l(_jb,_jg);});break;case 8:return new F(function(){return _l(_jc,_jg);});break;case 9:return new F(function(){return _l(_jd,_jg);});break;case 10:return new F(function(){return _l(_iX,_jg);});break;case 11:return new F(function(){return _l(_iY,_jg);});break;case 12:return new F(function(){return _l(_iZ,_jg);});break;case 13:return new F(function(){return _l(_j0,_jg);});break;case 14:return new F(function(){return _l(_j1,_jg);});break;case 15:return new F(function(){return _l(_j2,_jg);});break;case 16:return new F(function(){return _l(_j3,_jg);});break;case 17:return new F(function(){return _l(_j4,_jg);});break;default:return new F(function(){return _l(_j5,_jg);});}},_jh=[0,125],_ji=new T(function(){return B(unCStr("{handle: "));}),_jj=function(_jk,_jl,_jm,_jn,_jo,_jp){var _jq=new T(function(){var _jr=new T(function(){return B(_je(_jl,new T(function(){var _js=E(_jn);return _js[0]==0?E(_jp):B(_l(_iU,new T(function(){return B(_l(_js,[1,_iT,_jp]));})));})));}),_jt=E(_jm);return _jt[0]==0?E(_jr):B(_l(_jt,new T(function(){return B(_l(_iS,_jr));})));}),_ju=E(_jo);if(!_ju[0]){var _jv=E(_jk);if(!_jv[0]){return E(_jq);}else{var _jw=E(_jv[1]);return _jw[0]==0?B(_l(_ji,new T(function(){return B(_l(_jw[1],[1,_jh,new T(function(){return B(_l(_iS,_jq));})]));}))):B(_l(_ji,new T(function(){return B(_l(_jw[1],[1,_jh,new T(function(){return B(_l(_iS,_jq));})]));})));}}else{return new F(function(){return _l(_ju[1],new T(function(){return B(_l(_iS,_jq));}));});}},_jx=function(_jy){var _jz=E(_jy);return new F(function(){return _jj(_jz[1],_jz[2],_jz[3],_jz[4],_jz[6],_C);});},_jA=function(_jB,_jC){var _jD=E(_jB);return new F(function(){return _jj(_jD[1],_jD[2],_jD[3],_jD[4],_jD[6],_jC);});},_jE=function(_jF,_jG){return new F(function(){return _7m(_jA,_jF,_jG);});},_jH=function(_jI,_jJ,_jK){var _jL=E(_jJ);return new F(function(){return _jj(_jL[1],_jL[2],_jL[3],_jL[4],_jL[6],_jK);});},_jM=[0,_jH,_jx,_jE],_jN=new T(function(){return [0,_iN,_jM,_jO,_iP];}),_jO=function(_jP){return [0,_jN,_jP];},_jQ=function(_jR,_){return new F(function(){return die(new T(function(){return B(_jO(_jR));}));});},_jS=function(_jT,_){return new F(function(){return _jQ(_jT,_);});},_jU=new T(function(){return B(unCStr("iconvRecoder"));}),_jV=[0,-1],_jW=function(_jX,_jY,_jZ,_k0,_k1,_k2,_k3,_k4,_k5,_k6,_k7,_k8,_k9,_ka,_kb,_){var _kc=newByteArr(4),_kd=_kc,_ke=_kd,_kf=_ke,_kg=E(_k4)[1],_kh=function(_ki){var _kj=plusAddr(_jY,_ki),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_kk=newByteArr(4),_kl=_kk,_km=_kl,_kn=_km,_ko=E(_kb)[1],_kp=function(_kq){var _kr=plusAddr(_k5,_kq),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_ks=newByteArr(4),_kt=_ks,_ku=_kt,_kv=_ku,_kw=function(_kx){var _ky=_kv,_=writeOffAddr("w32",4,_ky,0,_kx),_kz=newByteArr(4),_kA=_kz,_kB=_kA,_kC=_kB,_kD=function(_kE){var _kF=_kC,_=writeOffAddr("w32",4,_kF,0,_kE),_kG=hs_iconv(E(_jX)[1],_kf,_ky,_kn,_kF),_kH=_kG,_kI=readOffAddr("w32",4,_ky,0),_kJ=_kI,_kK=readOffAddr("w32",4,_kF,0),_kL=_kK,_kM=new T(function(){if(_ko<32){var _kN=[0,(_kL&4294967295)>>_ko];}else{var _kN=(_kL&4294967295)>=0?E(_iG):E(_jV);}var _kO=_kN;return _kO;}),_kP=new T(function(){var _kQ=E(_kJ);if(!_kQ){var _kR=[0,_jY,_jZ,_k0,_k1,0,0];}else{if(_kg<32){var _kS=[0,_jY,_jZ,_k0,_k1,_k3-((_kQ&4294967295)>>_kg)|0,_k3];}else{if((_kQ&4294967295)>=0){var _kT=[0,_jY,_jZ,_k0,_k1,_k3,_k3];}else{var _kT=[0,_jY,_jZ,_k0,_k1,_k3+1|0,_k3];}var _kU=_kT,_kV=_kU,_kS=_kV;}var _kW=_kS,_kR=_kW;}return _kR;});if(_kH!=E(_iF)[1]){var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_iD,_kP,new T(function(){return [0,_k5,_k6,_k7,_k8,_k9,_k8-E(_kM)[1]|0];})];}else{var _kX=__hscore_get_errno(),_kY=_kX;switch(E(_kY)){case 7:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_iE,_kP,new T(function(){return [0,_k5,_k6,_k7,_k8,_k9,_k8-E(_kM)[1]|0];})];case 22:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_iD,_kP,new T(function(){return [0,_k5,_k6,_k7,_k8,_k9,_k8-E(_kM)[1]|0];})];case 84:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,new T(function(){return E(E(_kM)[1])==0?1:2;}),_kP,new T(function(){return [0,_k5,_k6,_k7,_k8,_k9,_k8-E(_kM)[1]|0];})];default:var _kZ=__hscore_get_errno(),_l0=_kZ;return new F(function(){return _jS(B(_iq(_jU,_l0,_fc,_fc)),_);});}}};if(_ko<32){return new F(function(){return _kD((_k8-_ka|0)<<_ko>>>0);});}else{return new F(function(){return _kD(0);});}};if(_kg<32){return new F(function(){return _kw((_k3-_k2|0)<<_kg>>>0);});}else{return new F(function(){return _kw(0);});}};if(_ko<32){return new F(function(){return _kp(_ka<<_ko);});}else{return new F(function(){return _kp(0);});}};if(_kg<32){return new F(function(){return _kh(_k2<<_kg);});}else{return new F(function(){return _kh(0);});}},_l1=[0,2],_l2=function(_l3,_l4,_l5,_){var _l6=E(_l4),_l7=E(_l5);return new F(function(){return _jW(_l3,_l6[1],_l6[2],_l6[3],_l6[4],_l6[5],_l6[6],_l1,_l7[1],_l7[2],_l7[3],_l7[4],_l7[5],_l7[6],_iG,_);});},_l8=function(_l9,_la,_lb,_){var _lc=E(_la),_ld=E(_lb);return new F(function(){return _jW(_l9,_lc[1],_lc[2],_lc[3],_lc[4],_lc[5],_lc[6],_iG,_ld[1],_ld[2],_ld[3],_ld[4],_ld[5],_ld[6],_l1,_);});},_le=function(_lf){return E(E(_lf)[1])==47?false:true;},_lg=function(_lh,_){return _Y;},_li=function(_){return _Y;},_lj=new T(function(){return B(unCStr("mkTextEncoding"));}),_lk=new T(function(){return B(unCStr("Iconv.close"));}),_ll=function(_lm,_ln,_){var _lo=newByteArr(B(_8V(_lm,0))+1|0),_lp=_lo,_lq=_lp,_lr=_lq,_ls=_lr,_lt=B((function(_lu,_lv,_){while(1){var _lw=E(_lu);if(!_lw[0]){var _=writeOffAddr("i8",1,_ls,_lv,0);return _Y;}else{var _=writeOffAddr("i8",1,_ls,_lv,E(_lw[1])[1]&255);_lu=_lw[2];var _lx=_lv+1|0;_lv=_lx;continue;}}})(_lm,0,_)),_ly=_lt,_lz=B(A(_ln,[[0,_ls],_])),_lA=_lz,_=0;return _lA;},_lB=function(_lC,_lD,_){return new F(function(){return _ll(_lC,_lD,_);});},_lE=function(_lF,_lG,_lH,_lI){return new F(function(){return _lB(_lF,function(_lJ){return new F(function(){return _lB(_lG,function(_lK,_){var _lL=hs_iconv_open(E(_lK)[1],E(_lJ)[1]),_lM=_lL,_lN=E(_lM);if(_lN==(-1)){var _lO=__hscore_get_errno(),_lP=_lO;return new F(function(){return _jS(B(_iq(_lj,_lP,_fc,_fc)),_);});}else{return [0,new T(function(){return B(A(_lI,[[0,_lN]]));}),_lH,function(_){var _lQ=hs_iconv_close(_lN),_lR=_lQ;if(E(_lR)==(-1)){var _lS=__hscore_get_errno(),_lT=_lS;return new F(function(){return _jS(B(_iq(_lk,_lT,_fc,_fc)),_);});}else{return _Y;}},_li,_lg];}});});});});},_lU=function(_jT,_){return new F(function(){return _jQ(_jT,_);});},_lV=12,_lW=new T(function(){return B(unCStr("invalid byte sequence"));}),_lX=new T(function(){return B(unCStr("recoverDecode"));}),_lY=[0,_fc,_lV,_lX,_lW,_fc,_fc],_lZ=function(_m0,_m1,_m2,_m3,_m4,_m5,_m6,_m7,_m8,_m9,_ma,_mb,_mc,_){switch(E(_m0)){case 0:return new F(function(){return _lU(_lY,_);});break;case 1:return [0,[0,_m1,_m2,_m3,_m4,_m5+1|0,_m6],[0,_m7,_m8,_m9,_ma,_mb,_mc]];case 2:var _=writeOffAddr("w32",4,_m7,_mc,65533),_=0;return [0,[0,_m1,_m2,_m3,_m4,_m5+1|0,_m6],[0,_m7,_m8,_m9,_ma,_mb,_mc+1|0]];default:var _md=readOffAddr("w8",1,plusAddr(_m1,_m5),0),_me=_md,_=0;if(_me>=128){var _mf=56320+(_me&4294967295)|0;if(_mf>>>0>1114111){return new F(function(){return _D(_mf);});}else{var _=writeOffAddr("w32",4,_m7,_mc,_mf),_=0;return [0,[0,_m1,_m2,_m3,_m4,_m5+1|0,_m6],[0,_m7,_m8,_m9,_ma,_mb,_mc+1|0]];}}else{var _mg=_me&4294967295;if(_mg>>>0>1114111){return new F(function(){return _D(_mg);});}else{var _=writeOffAddr("w32",4,_m7,_mc,_mg),_=0;return [0,[0,_m1,_m2,_m3,_m4,_m5+1|0,_m6],[0,_m7,_m8,_m9,_ma,_mb,_mc+1|0]];}}}},_mh=function(_mi,_mj,_mk,_){var _ml=E(_mj),_mm=E(_mk);return new F(function(){return _lZ(_mi,_ml[1],_ml[2],_ml[3],_ml[4],_ml[5],_ml[6],_mm[1],_mm[2],_mm[3],_mm[4],_mm[5],_mm[6],_);});},_mn=new T(function(){return B(unCStr("recoverEncode"));}),_mo=new T(function(){return B(unCStr("invalid character"));}),_mp=[0,_fc,_lV,_mn,_mo,_fc,_fc],_mq=function(_){return new F(function(){return _lU(_mp,_);});},_mr=function(_ms,_mt,_mu,_mv,_mw,_mx,_my,_mz,_mA,_mB,_mC,_mD,_mE,_){var _mF=readOffAddr("w32",4,_mt,_mx),_mG=_mF,_=0;switch(E(_ms)){case 0:return new F(function(){return _mq(_);});break;case 1:return [0,[0,_mt,_mu,_mv,_mw,_mx+1|0,_my],[0,_mz,_mA,_mB,_mC,_mD,_mE]];case 2:if(E(_mG)==63){return [0,[0,_mt,_mu,_mv,_mw,_mx+1|0,_my],[0,_mz,_mA,_mB,_mC,_mD,_mE]];}else{var _=writeOffAddr("w32",4,_mt,_mx,63),_=0;return [0,[0,_mt,_mu,_mv,_mw,_mx,_my],[0,_mz,_mA,_mB,_mC,_mD,_mE]];}break;default:var _mH=_mG;if(56448>_mH){return new F(function(){return _mq(_);});}else{if(_mH>=56576){return new F(function(){return _mq(_);});}else{var _=writeOffAddr("w8",1,plusAddr(_mz,_mE),0,_mH>>>0&255),_=0;return [0,[0,_mt,_mu,_mv,_mw,_mx+1|0,_my],[0,_mz,_mA,_mB,_mC,_mD,_mE+1|0]];}}}},_mI=function(_mJ,_mK,_mL,_){var _mM=E(_mK),_mN=E(_mL);return new F(function(){return _mr(_mJ,_mM[1],_mM[2],_mM[3],_mM[4],_mM[5],_mM[6],_mN[1],_mN[2],_mN[3],_mN[4],_mN[5],_mN[6],_);});},_mO=function(_mP,_mQ,_){return [0,_mQ,new T(function(){var _mR=new T(function(){var _mS=B(_4M(_le,_mQ));return [0,_mS[1],_mS[2]];});return B(_lE(new T(function(){return E(E(_mR)[1]);}),new T(function(){return B(_l(_hn,new T(function(){return E(E(_mR)[2]);})));}),function(_mT,_mU,_){return new F(function(){return _mh(_mP,_mT,_mU,_);});},_l8));}),new T(function(){return B(_lE(_hn,_mQ,function(_mT,_mU,_){return new F(function(){return _mI(_mP,_mT,_mU,_);});},_l2));})];},_mV=2,_mW=function(_mX,_mY,_mZ,_n0,_n1,_n2,_n3,_n4,_n5,_n6,_n7,_n8,_){var _n9=[0,_mX,_mY,_mZ,_n0,0,0],_na=function(_nb,_nc,_){while(1){var _nd=(function(_ne,_nf,_){if(_ne<_n2){if((_n6-_nf|0)>=2){var _ng=readOffAddr("w32",4,_mX,_ne),_nh=_ng,_=0,_ni=_nh;if(_ni>=65536){if((_n6-_nf|0)>=4){var _nj=_ni-65536|0,_=writeOffAddr("w8",1,plusAddr(_n3,_nf),0,((_nj>>18)+216|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_n3,_nf+1|0),0,_nj>>10>>>0&255),_=0,_nk=(_nj>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_n3,_nf+2|0),0,((_nk>>8)+220|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_n3,_nf+3|0),0,_nk>>>0&255),_=0,_nl=_ne+1|0,_nm=_nf+4|0;_nb=_nl;_nc=_nm;return null;}else{return [0,_iE,new T(function(){return _ne!=_n2?[0,_mX,_mY,_mZ,_n0,_ne,_n2]:E(_n9);}),[0,_n3,_n4,_n5,_n6,_n7,_nf]];}}else{var _nn=function(_no){if(56320>_ni){var _=writeOffAddr("w8",1,plusAddr(_n3,_nf),0,_ni>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_n3,_nf+1|0),0,_ni>>>0&255),_=0;return new F(function(){return _na(_ne+1|0,_nf+2|0,_);});}else{if(_ni>57343){var _=writeOffAddr("w8",1,plusAddr(_n3,_nf),0,_ni>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_n3,_nf+1|0),0,_ni>>>0&255),_=0;return new F(function(){return _na(_ne+1|0,_nf+2|0,_);});}else{return [0,_mV,new T(function(){return _ne!=_n2?[0,_mX,_mY,_mZ,_n0,_ne,_n2]:E(_n9);}),[0,_n3,_n4,_n5,_n6,_n7,_nf]];}}};if(55296>_ni){return new F(function(){return _nn(_);});}else{return _ni>56319?B(_nn(_)):[0,_mV,new T(function(){return _ne!=_n2?[0,_mX,_mY,_mZ,_n0,_ne,_n2]:E(_n9);}),[0,_n3,_n4,_n5,_n6,_n7,_nf]];}}}else{return [0,_iE,new T(function(){return _ne!=_n2?[0,_mX,_mY,_mZ,_n0,_ne,_n2]:E(_n9);}),[0,_n3,_n4,_n5,_n6,_n7,_nf]];}}else{return [0,_iD,new T(function(){return _ne!=_n2?[0,_mX,_mY,_mZ,_n0,_ne,_n2]:E(_n9);}),[0,_n3,_n4,_n5,_n6,_n7,_nf]];}})(_nb,_nc,_);if(_nd!=null){return _nd;}}};return new F(function(){return _na(_n1,_n8,_);});},_np=function(_nq,_nr,_ns,_nt,_nu,_nv,_nw,_nx,_){var _ny=rMV(_nq),_nz=_ny;if(!E(_nz)){if((_nv-_nx|0)>=2){var _=wMV(_nq,_ae),_=writeOffAddr("w8",1,plusAddr(_ns,_nx),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_ns,_nx+1|0),0,255),_=0,_nA=E(_nr);return new F(function(){return _mW(_nA[1],_nA[2],_nA[3],_nA[4],_nA[5],_nA[6],_ns,_nt,_nu,_nv,_nw,_nx+2|0,_);});}else{return [0,_iE,_nr,[0,_ns,_nt,_nu,_nv,_nw,_nx]];}}else{var _nB=E(_nr);return new F(function(){return _mW(_nB[1],_nB[2],_nB[3],_nB[4],_nB[5],_nB[6],_ns,_nt,_nu,_nv,_nw,_nx,_);});}},_nC=function(_nD,_nE,_nF,_nG,_nH,_nI,_nJ,_nK,_nL,_nM,_nN,_nO,_){var _nP=[0,_nD,_nE,_nF,_nG,0,0];return new F(function(){return (function(_nQ,_nR,_){while(1){var _nS=(function(_nT,_nU,_){if(_nU<_nM){if(_nT<_nI){if((_nT+1|0)!=_nI){var _nV=readOffAddr("w8",1,plusAddr(_nD,_nT),0),_nW=_nV,_=0,_nX=readOffAddr("w8",1,plusAddr(_nD,_nT+1|0),0),_nY=_nX,_=0,_nZ=(_nW<<8>>>0&65535)+_nY>>>0&65535;if(_nZ>=55296){if(_nZ<=57343){if((_nI-_nT|0)>=4){var _o0=readOffAddr("w8",1,plusAddr(_nD,_nT+2|0),0),_o1=_o0,_=0,_o2=readOffAddr("w8",1,plusAddr(_nD,_nT+3|0),0),_o3=_o2,_=0;if(_nZ<55296){return [0,_mV,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}else{if(_nZ>56319){return [0,_mV,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}else{var _o4=(_o1<<8>>>0&65535)+_o3>>>0&65535;if(_o4<56320){return [0,_mV,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}else{if(_o4>57343){return [0,_mV,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}else{var _=writeOffAddr("w32",4,_nJ,_nU,((((_nZ&4294967295)-55296|0)<<10)+((_o4&4294967295)-56320|0)|0)+65536|0),_=0,_o5=_nT+4|0,_o6=_nU+1|0;_nQ=_o5;_nR=_o6;return null;}}}}}else{return [0,_iD,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}}else{var _=writeOffAddr("w32",4,_nJ,_nU,_nZ&4294967295),_=0,_o5=_nT+2|0,_o6=_nU+1|0;_nQ=_o5;_nR=_o6;return null;}}else{var _=writeOffAddr("w32",4,_nJ,_nU,_nZ&4294967295),_=0,_o5=_nT+2|0,_o6=_nU+1|0;_nQ=_o5;_nR=_o6;return null;}}else{return [0,_iD,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}}else{return [0,_iD,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}}else{return [0,_iE,new T(function(){return _nT!=_nI?[0,_nD,_nE,_nF,_nG,_nT,_nI]:E(_nP);}),[0,_nJ,_nK,_nL,_nM,_nN,_nU]];}})(_nQ,_nR,_);if(_nS!=null){return _nS;}}})(_nH,_nO,_);});},_o7=function(_o8,_o9,_oa,_ob,_oc,_od,_oe,_of,_og,_oh,_oi,_oj,_){var _ok=[0,_o8,_o9,_oa,_ob,0,0];return new F(function(){return (function(_ol,_om,_){while(1){var _on=(function(_oo,_op,_){if(_op<_oh){if(_oo<_od){if((_oo+1|0)!=_od){var _oq=readOffAddr("w8",1,plusAddr(_o8,_oo),0),_or=_oq,_=0,_os=readOffAddr("w8",1,plusAddr(_o8,_oo+1|0),0),_ot=_os,_=0,_ou=(_ot<<8>>>0&65535)+_or>>>0&65535;if(_ou>=55296){if(_ou<=57343){if((_od-_oo|0)>=4){var _ov=readOffAddr("w8",1,plusAddr(_o8,_oo+2|0),0),_ow=_ov,_=0,_ox=readOffAddr("w8",1,plusAddr(_o8,_oo+3|0),0),_oy=_ox,_=0;if(_ou<55296){return [0,_mV,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}else{if(_ou>56319){return [0,_mV,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}else{var _oz=(_oy<<8>>>0&65535)+_ow>>>0&65535;if(_oz<56320){return [0,_mV,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}else{if(_oz>57343){return [0,_mV,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}else{var _=writeOffAddr("w32",4,_oe,_op,((((_ou&4294967295)-55296|0)<<10)+((_oz&4294967295)-56320|0)|0)+65536|0),_=0,_oA=_oo+4|0,_oB=_op+1|0;_ol=_oA;_om=_oB;return null;}}}}}else{return [0,_iD,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}}else{var _=writeOffAddr("w32",4,_oe,_op,_ou&4294967295),_=0,_oA=_oo+2|0,_oB=_op+1|0;_ol=_oA;_om=_oB;return null;}}else{var _=writeOffAddr("w32",4,_oe,_op,_ou&4294967295),_=0,_oA=_oo+2|0,_oB=_op+1|0;_ol=_oA;_om=_oB;return null;}}else{return [0,_iD,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}}else{return [0,_iD,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}}else{return [0,_iE,new T(function(){return _oo!=_od?[0,_o8,_o9,_oa,_ob,_oo,_od]:E(_ok);}),[0,_oe,_of,_og,_oh,_oi,_op]];}})(_ol,_om,_);if(_on!=null){return _on;}}})(_oc,_oj,_);});},_oC=function(_oD,_oE,_){var _oF=E(_oD),_oG=E(_oE);return new F(function(){return _nC(_oF[1],_oF[2],_oF[3],_oF[4],_oF[5],_oF[6],_oG[1],_oG[2],_oG[3],_oG[4],_oG[5],_oG[6],_);});},_oH=[1,_oC],_oI=function(_oJ,_oK,_){var _oL=E(_oJ),_oM=E(_oK);return new F(function(){return _o7(_oL[1],_oL[2],_oL[3],_oL[4],_oL[5],_oL[6],_oM[1],_oM[2],_oM[3],_oM[4],_oM[5],_oM[6],_);});},_oN=[1,_oI],_oO=function(_oP,_oQ,_oR,_oS,_oT,_oU,_oV,_oW,_){var _oX=rMV(_oP),_oY=_oX,_oZ=E(_oY);if(!_oZ[0]){if((_oV-_oU|0)>=2){var _p0=readOffAddr("w8",1,plusAddr(_oQ,_oU),0),_p1=_p0,_=0,_p2=readOffAddr("w8",1,plusAddr(_oQ,_oU+1|0),0),_p3=_p2,_=0,_p4=function(_p5){if(E(_p1)==255){if(E(_p3)==254){var _=wMV(_oP,_oN),_p6=E(_oW);return new F(function(){return _o7(_oQ,_oR,_oS,_oT,_oU+2|0,_oV,_p6[1],_p6[2],_p6[3],_p6[4],_p6[5],_p6[6],_);});}else{var _=wMV(_oP,_oH),_p7=E(_oW);return new F(function(){return _nC(_oQ,_oR,_oS,_oT,_oU,_oV,_p7[1],_p7[2],_p7[3],_p7[4],_p7[5],_p7[6],_);});}}else{var _=wMV(_oP,_oH),_p8=E(_oW);return new F(function(){return _nC(_oQ,_oR,_oS,_oT,_oU,_oV,_p8[1],_p8[2],_p8[3],_p8[4],_p8[5],_p8[6],_);});}};if(E(_p1)==254){if(E(_p3)==255){var _=wMV(_oP,_oH),_p9=E(_oW);return new F(function(){return _nC(_oQ,_oR,_oS,_oT,_oU+2|0,_oV,_p9[1],_p9[2],_p9[3],_p9[4],_p9[5],_p9[6],_);});}else{return new F(function(){return _p4(_);});}}else{return new F(function(){return _p4(_);});}}else{return [0,_iD,[0,_oQ,_oR,_oS,_oT,_oU,_oV],_oW];}}else{return new F(function(){return A(_oZ[1],[[0,_oQ,_oR,_oS,_oT,_oU,_oV],_oW,_]);});}},_pa=false,_pb=function(_){return _Y;},_pc=new T(function(){return B(unCStr("UTF-16"));}),_pd=function(_pe){return [0,_pc,function(_){var _pf=nMV(_fc),_pg=_pf;return [0,function(_ph,_pi,_){var _pj=E(_ph);return new F(function(){return _oO(_pg,_pj[1],_pj[2],_pj[3],_pj[4],_pj[5],_pj[6],_pi,_);});},function(_pk,_pl,_){return new F(function(){return _mh(_pe,_pk,_pl,_);});},_pb,function(_){return new F(function(){return rMV(_pg);});},function(_pm,_){var _=wMV(_pg,_pm);return _Y;}];},function(_){var _pn=nMV(_pa),_po=_pn;return [0,function(_pp,_pq,_){var _pr=E(_pq);return new F(function(){return _np(_po,_pp,_pr[1],_pr[2],_pr[3],_pr[4],_pr[5],_pr[6],_);});},function(_pk,_pl,_){return new F(function(){return _mI(_pe,_pk,_pl,_);});},_pb,function(_){return new F(function(){return rMV(_po);});},function(_ps,_){var _=wMV(_po,_ps);return _Y;}];}];},_pt=function(_pu,_pv,_){var _pw=E(_pu),_px=E(_pv);return new F(function(){return _mW(_pw[1],_pw[2],_pw[3],_pw[4],_pw[5],_pw[6],_px[1],_px[2],_px[3],_px[4],_px[5],_px[6],_);});},_py=function(_pz,_){return _Y;},_pA=new T(function(){return B(unCStr("UTF-16BE"));}),_pB=function(_pC){return [0,_pA,function(_){return [0,_oC,function(_pk,_pl,_){return new F(function(){return _mh(_pC,_pk,_pl,_);});},_pb,_pb,_py];},function(_){return [0,_pt,function(_pk,_pl,_){return new F(function(){return _mI(_pC,_pk,_pl,_);});},_pb,_pb,_py];}];},_pD=function(_pE,_pF,_pG,_pH,_pI,_pJ,_pK,_pL,_pM,_pN,_pO,_pP,_){var _pQ=[0,_pE,_pF,_pG,_pH,0,0],_pR=function(_pS,_pT,_){while(1){var _pU=(function(_pV,_pW,_){if(_pV<_pJ){if((_pN-_pW|0)>=2){var _pX=readOffAddr("w32",4,_pE,_pV),_pY=_pX,_=0,_pZ=_pY;if(_pZ>=65536){if((_pN-_pW|0)>=4){var _q0=_pZ-65536|0,_=writeOffAddr("w8",1,plusAddr(_pK,_pW),0,_q0>>10>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_pK,_pW+1|0),0,((_q0>>18)+216|0)>>>0&255),_=0,_q1=(_q0>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_pK,_pW+2|0),0,_q1>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_pK,_pW+3|0),0,((_q1>>8)+220|0)>>>0&255),_=0,_q2=_pV+1|0,_q3=_pW+4|0;_pS=_q2;_pT=_q3;return null;}else{return [0,_iE,new T(function(){return _pV!=_pJ?[0,_pE,_pF,_pG,_pH,_pV,_pJ]:E(_pQ);}),[0,_pK,_pL,_pM,_pN,_pO,_pW]];}}else{var _q4=function(_q5){if(56320>_pZ){var _=writeOffAddr("w8",1,plusAddr(_pK,_pW),0,_pZ>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_pK,_pW+1|0),0,_pZ>>8>>>0&255),_=0;return new F(function(){return _pR(_pV+1|0,_pW+2|0,_);});}else{if(_pZ>57343){var _=writeOffAddr("w8",1,plusAddr(_pK,_pW),0,_pZ>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_pK,_pW+1|0),0,_pZ>>8>>>0&255),_=0;return new F(function(){return _pR(_pV+1|0,_pW+2|0,_);});}else{return [0,_mV,new T(function(){return _pV!=_pJ?[0,_pE,_pF,_pG,_pH,_pV,_pJ]:E(_pQ);}),[0,_pK,_pL,_pM,_pN,_pO,_pW]];}}};if(55296>_pZ){return new F(function(){return _q4(_);});}else{return _pZ>56319?B(_q4(_)):[0,_mV,new T(function(){return _pV!=_pJ?[0,_pE,_pF,_pG,_pH,_pV,_pJ]:E(_pQ);}),[0,_pK,_pL,_pM,_pN,_pO,_pW]];}}}else{return [0,_iE,new T(function(){return _pV!=_pJ?[0,_pE,_pF,_pG,_pH,_pV,_pJ]:E(_pQ);}),[0,_pK,_pL,_pM,_pN,_pO,_pW]];}}else{return [0,_iD,new T(function(){return _pV!=_pJ?[0,_pE,_pF,_pG,_pH,_pV,_pJ]:E(_pQ);}),[0,_pK,_pL,_pM,_pN,_pO,_pW]];}})(_pS,_pT,_);if(_pU!=null){return _pU;}}};return new F(function(){return _pR(_pI,_pP,_);});},_q6=function(_q7,_q8,_){var _q9=E(_q7),_qa=E(_q8);return new F(function(){return _pD(_q9[1],_q9[2],_q9[3],_q9[4],_q9[5],_q9[6],_qa[1],_qa[2],_qa[3],_qa[4],_qa[5],_qa[6],_);});},_qb=new T(function(){return B(unCStr("UTF16-LE"));}),_qc=function(_qd){return [0,_qb,function(_){return [0,_oI,function(_pk,_pl,_){return new F(function(){return _mh(_qd,_pk,_pl,_);});},_pb,_pb,_py];},function(_){return [0,_q6,function(_pk,_pl,_){return new F(function(){return _mI(_qd,_pk,_pl,_);});},_pb,_pb,_py];}];},_qe=function(_qf,_qg,_qh,_qi,_qj,_qk,_ql,_qm,_qn,_qo,_qp,_qq,_){var _qr=[0,_qf,_qg,_qh,_qi,0,0],_qs=function(_qt,_qu,_){if(_qt<_qk){if((_qo-_qu|0)>=4){var _qv=readOffAddr("w32",4,_qf,_qt),_qw=_qv,_=0,_qx=_qw,_qy=function(_qz){if(56320>_qx){var _=writeOffAddr("w8",1,plusAddr(_ql,_qu),0,_qx>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+1|0),0,_qx>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+2|0),0,_qx>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+3|0),0,_qx>>>0&255),_=0;return new F(function(){return _qs(_qt+1|0,_qu+4|0,_);});}else{if(_qx>57343){var _=writeOffAddr("w8",1,plusAddr(_ql,_qu),0,_qx>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+1|0),0,_qx>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+2|0),0,_qx>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_ql,_qu+3|0),0,_qx>>>0&255),_=0;return new F(function(){return _qs(_qt+1|0,_qu+4|0,_);});}else{return [0,_mV,new T(function(){return _qt!=_qk?[0,_qf,_qg,_qh,_qi,_qt,_qk]:E(_qr);}),[0,_ql,_qm,_qn,_qo,_qp,_qu]];}}};if(55296>_qx){return new F(function(){return _qy(_);});}else{return _qx>56319?B(_qy(_)):[0,_mV,new T(function(){return _qt!=_qk?[0,_qf,_qg,_qh,_qi,_qt,_qk]:E(_qr);}),[0,_ql,_qm,_qn,_qo,_qp,_qu]];}}else{return [0,_iE,new T(function(){return _qt!=_qk?[0,_qf,_qg,_qh,_qi,_qt,_qk]:E(_qr);}),[0,_ql,_qm,_qn,_qo,_qp,_qu]];}}else{return [0,_iD,new T(function(){return _qt!=_qk?[0,_qf,_qg,_qh,_qi,_qt,_qk]:E(_qr);}),[0,_ql,_qm,_qn,_qo,_qp,_qu]];}};return new F(function(){return _qs(_qj,_qq,_);});},_qA=function(_qB,_qC,_qD,_qE,_qF,_qG,_qH,_qI,_){var _qJ=rMV(_qB),_qK=_qJ;if(!E(_qK)){if((_qG-_qI|0)>=4){var _=wMV(_qB,_ae),_=writeOffAddr("w8",1,plusAddr(_qD,_qI),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_qD,_qI+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_qD,_qI+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_qD,_qI+3|0),0,255),_=0,_qL=E(_qC);return new F(function(){return _qe(_qL[1],_qL[2],_qL[3],_qL[4],_qL[5],_qL[6],_qD,_qE,_qF,_qG,_qH,_qI+4|0,_);});}else{return [0,_iE,_qC,[0,_qD,_qE,_qF,_qG,_qH,_qI]];}}else{var _qM=E(_qC);return new F(function(){return _qe(_qM[1],_qM[2],_qM[3],_qM[4],_qM[5],_qM[6],_qD,_qE,_qF,_qG,_qH,_qI,_);});}},_qN=function(_qO,_qP,_qQ,_qR,_qS,_qT,_qU,_qV,_qW,_qX,_qY,_qZ,_){var _r0=[0,_qO,_qP,_qQ,_qR,0,0],_r1=function(_r2,_r3,_){while(1){var _r4=(function(_r5,_r6,_){if(_r6<_qX){if((_qT-_r5|0)>=4){var _r7=readOffAddr("w8",1,plusAddr(_qO,_r5),0),_r8=_r7,_=0,_r9=readOffAddr("w8",1,plusAddr(_qO,_r5+1|0),0),_ra=_r9,_=0,_rb=readOffAddr("w8",1,plusAddr(_qO,_r5+2|0),0),_rc=_rb,_=0,_rd=readOffAddr("w8",1,plusAddr(_qO,_r5+3|0),0),_re=_rd,_=0,_rf=((((_r8&4294967295)<<24)+((_ra&4294967295)<<16)|0)+((_rc&4294967295)<<8)|0)+(_re&4294967295)|0,_rg=_rf,_rh=function(_ri){if(_rg<=57343){return [0,_mV,new T(function(){return _r5!=_qT?[0,_qO,_qP,_qQ,_qR,_r5,_qT]:E(_r0);}),[0,_qU,_qV,_qW,_qX,_qY,_r6]];}else{if(_rg>1114111){return [0,_mV,new T(function(){return _r5!=_qT?[0,_qO,_qP,_qQ,_qR,_r5,_qT]:E(_r0);}),[0,_qU,_qV,_qW,_qX,_qY,_r6]];}else{var _=writeOffAddr("w32",4,_qU,_r6,_rf),_=0;return new F(function(){return _r1(_r5+4|0,_r6+1|0,_);});}}};if(_rg<0){return new F(function(){return _rh(_);});}else{if(_rg>=55296){return new F(function(){return _rh(_);});}else{var _=writeOffAddr("w32",4,_qU,_r6,_rf),_=0,_rj=_r5+4|0,_rk=_r6+1|0;_r2=_rj;_r3=_rk;return null;}}}else{return [0,_iD,new T(function(){return _r5!=_qT?[0,_qO,_qP,_qQ,_qR,_r5,_qT]:E(_r0);}),[0,_qU,_qV,_qW,_qX,_qY,_r6]];}}else{return [0,_iE,new T(function(){return _r5!=_qT?[0,_qO,_qP,_qQ,_qR,_r5,_qT]:E(_r0);}),[0,_qU,_qV,_qW,_qX,_qY,_r6]];}})(_r2,_r3,_);if(_r4!=null){return _r4;}}};return new F(function(){return _r1(_qS,_qZ,_);});},_rl=function(_rm,_rn,_ro,_rp,_rq,_rr,_rs,_rt,_ru,_rv,_rw,_rx,_){var _ry=[0,_rm,_rn,_ro,_rp,0,0],_rz=function(_rA,_rB,_){while(1){var _rC=(function(_rD,_rE,_){if(_rE<_rv){if((_rr-_rD|0)>=4){var _rF=readOffAddr("w8",1,plusAddr(_rm,_rD),0),_rG=_rF,_=0,_rH=readOffAddr("w8",1,plusAddr(_rm,_rD+1|0),0),_rI=_rH,_=0,_rJ=readOffAddr("w8",1,plusAddr(_rm,_rD+2|0),0),_rK=_rJ,_=0,_rL=readOffAddr("w8",1,plusAddr(_rm,_rD+3|0),0),_rM=_rL,_=0,_rN=((((_rM&4294967295)<<24)+((_rK&4294967295)<<16)|0)+((_rI&4294967295)<<8)|0)+(_rG&4294967295)|0,_rO=_rN,_rP=function(_rQ){if(_rO<=57343){return [0,_mV,new T(function(){return _rD!=_rr?[0,_rm,_rn,_ro,_rp,_rD,_rr]:E(_ry);}),[0,_rs,_rt,_ru,_rv,_rw,_rE]];}else{if(_rO>1114111){return [0,_mV,new T(function(){return _rD!=_rr?[0,_rm,_rn,_ro,_rp,_rD,_rr]:E(_ry);}),[0,_rs,_rt,_ru,_rv,_rw,_rE]];}else{var _=writeOffAddr("w32",4,_rs,_rE,_rN),_=0;return new F(function(){return _rz(_rD+4|0,_rE+1|0,_);});}}};if(_rO<0){return new F(function(){return _rP(_);});}else{if(_rO>=55296){return new F(function(){return _rP(_);});}else{var _=writeOffAddr("w32",4,_rs,_rE,_rN),_=0,_rR=_rD+4|0,_rS=_rE+1|0;_rA=_rR;_rB=_rS;return null;}}}else{return [0,_iD,new T(function(){return _rD!=_rr?[0,_rm,_rn,_ro,_rp,_rD,_rr]:E(_ry);}),[0,_rs,_rt,_ru,_rv,_rw,_rE]];}}else{return [0,_iE,new T(function(){return _rD!=_rr?[0,_rm,_rn,_ro,_rp,_rD,_rr]:E(_ry);}),[0,_rs,_rt,_ru,_rv,_rw,_rE]];}})(_rA,_rB,_);if(_rC!=null){return _rC;}}};return new F(function(){return _rz(_rq,_rx,_);});},_rT=function(_rU,_rV,_){var _rW=E(_rU),_rX=E(_rV);return new F(function(){return _qN(_rW[1],_rW[2],_rW[3],_rW[4],_rW[5],_rW[6],_rX[1],_rX[2],_rX[3],_rX[4],_rX[5],_rX[6],_);});},_rY=[1,_rT],_rZ=function(_s0,_s1,_){var _s2=E(_s0),_s3=E(_s1);return new F(function(){return _rl(_s2[1],_s2[2],_s2[3],_s2[4],_s2[5],_s2[6],_s3[1],_s3[2],_s3[3],_s3[4],_s3[5],_s3[6],_);});},_s4=[1,_rZ],_s5=function(_s6,_s7,_s8,_s9,_sa,_sb,_sc,_sd,_){var _se=rMV(_s6),_sf=_se,_sg=E(_sf);if(!_sg[0]){if((_sc-_sb|0)>=4){var _sh=readOffAddr("w8",1,plusAddr(_s7,_sb),0),_si=_sh,_=0,_sj=readOffAddr("w8",1,plusAddr(_s7,_sb+1|0),0),_sk=_sj,_=0,_sl=readOffAddr("w8",1,plusAddr(_s7,_sb+2|0),0),_sm=_sl,_=0,_sn=readOffAddr("w8",1,plusAddr(_s7,_sb+3|0),0),_so=_sn,_=0,_sp=function(_sq){if(E(_si)==255){if(E(_sk)==254){if(!E(_sm)){if(!E(_so)){var _=wMV(_s6,_s4),_sr=E(_sd);return new F(function(){return _rl(_s7,_s8,_s9,_sa,_sb+4|0,_sc,_sr[1],_sr[2],_sr[3],_sr[4],_sr[5],_sr[6],_);});}else{var _=wMV(_s6,_rY),_ss=E(_sd);return new F(function(){return _qN(_s7,_s8,_s9,_sa,_sb,_sc,_ss[1],_ss[2],_ss[3],_ss[4],_ss[5],_ss[6],_);});}}else{var _=wMV(_s6,_rY),_st=E(_sd);return new F(function(){return _qN(_s7,_s8,_s9,_sa,_sb,_sc,_st[1],_st[2],_st[3],_st[4],_st[5],_st[6],_);});}}else{var _=wMV(_s6,_rY),_su=E(_sd);return new F(function(){return _qN(_s7,_s8,_s9,_sa,_sb,_sc,_su[1],_su[2],_su[3],_su[4],_su[5],_su[6],_);});}}else{var _=wMV(_s6,_rY),_sv=E(_sd);return new F(function(){return _qN(_s7,_s8,_s9,_sa,_sb,_sc,_sv[1],_sv[2],_sv[3],_sv[4],_sv[5],_sv[6],_);});}};if(!E(_si)){if(!E(_sk)){if(E(_sm)==254){if(E(_so)==255){var _=wMV(_s6,_rY),_sw=E(_sd);return new F(function(){return _qN(_s7,_s8,_s9,_sa,_sb+4|0,_sc,_sw[1],_sw[2],_sw[3],_sw[4],_sw[5],_sw[6],_);});}else{return new F(function(){return _sp(_);});}}else{return new F(function(){return _sp(_);});}}else{return new F(function(){return _sp(_);});}}else{return new F(function(){return _sp(_);});}}else{return [0,_iD,[0,_s7,_s8,_s9,_sa,_sb,_sc],_sd];}}else{return new F(function(){return A(_sg[1],[[0,_s7,_s8,_s9,_sa,_sb,_sc],_sd,_]);});}},_sx=function(_){return _Y;},_sy=new T(function(){return B(unCStr("UTF-32"));}),_sz=function(_sA){return [0,_sy,function(_){var _sB=nMV(_fc),_sC=_sB;return [0,function(_sD,_sE,_){var _sF=E(_sD);return new F(function(){return _s5(_sC,_sF[1],_sF[2],_sF[3],_sF[4],_sF[5],_sF[6],_sE,_);});},function(_sG,_sH,_){return new F(function(){return _mh(_sA,_sG,_sH,_);});},_sx,function(_){return new F(function(){return rMV(_sC);});},function(_sI,_){var _=wMV(_sC,_sI);return _Y;}];},function(_){var _sJ=nMV(_pa),_sK=_sJ;return [0,function(_sL,_sM,_){var _sN=E(_sM);return new F(function(){return _qA(_sK,_sL,_sN[1],_sN[2],_sN[3],_sN[4],_sN[5],_sN[6],_);});},function(_sG,_sH,_){return new F(function(){return _mI(_sA,_sG,_sH,_);});},_sx,function(_){return new F(function(){return rMV(_sK);});},function(_sO,_){var _=wMV(_sK,_sO);return _Y;}];}];},_sP=function(_sQ,_sR,_){var _sS=E(_sQ),_sT=E(_sR);return new F(function(){return _qe(_sS[1],_sS[2],_sS[3],_sS[4],_sS[5],_sS[6],_sT[1],_sT[2],_sT[3],_sT[4],_sT[5],_sT[6],_);});},_sU=function(_sV,_){return _Y;},_sW=new T(function(){return B(unCStr("UTF-32BE"));}),_sX=function(_sY){return [0,_sW,function(_){return [0,_rT,function(_sG,_sH,_){return new F(function(){return _mh(_sY,_sG,_sH,_);});},_sx,_sx,_sU];},function(_){return [0,_sP,function(_sG,_sH,_){return new F(function(){return _mI(_sY,_sG,_sH,_);});},_sx,_sx,_sU];}];},_sZ=function(_t0,_t1,_t2,_t3,_t4,_t5,_t6,_t7,_t8,_t9,_ta,_tb,_){var _tc=[0,_t0,_t1,_t2,_t3,0,0],_td=function(_te,_tf,_){if(_te<_t5){if((_t9-_tf|0)>=4){var _tg=readOffAddr("w32",4,_t0,_te),_th=_tg,_=0,_ti=_th,_tj=function(_tk){if(56320>_ti){var _=writeOffAddr("w8",1,plusAddr(_t6,_tf),0,_ti>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+1|0),0,_ti>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+2|0),0,_ti>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+3|0),0,_ti>>24>>>0&255),_=0;return new F(function(){return _td(_te+1|0,_tf+4|0,_);});}else{if(_ti>57343){var _=writeOffAddr("w8",1,plusAddr(_t6,_tf),0,_ti>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+1|0),0,_ti>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+2|0),0,_ti>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_t6,_tf+3|0),0,_ti>>24>>>0&255),_=0;return new F(function(){return _td(_te+1|0,_tf+4|0,_);});}else{return [0,_mV,new T(function(){return _te!=_t5?[0,_t0,_t1,_t2,_t3,_te,_t5]:E(_tc);}),[0,_t6,_t7,_t8,_t9,_ta,_tf]];}}};if(55296>_ti){return new F(function(){return _tj(_);});}else{return _ti>56319?B(_tj(_)):[0,_mV,new T(function(){return _te!=_t5?[0,_t0,_t1,_t2,_t3,_te,_t5]:E(_tc);}),[0,_t6,_t7,_t8,_t9,_ta,_tf]];}}else{return [0,_iE,new T(function(){return _te!=_t5?[0,_t0,_t1,_t2,_t3,_te,_t5]:E(_tc);}),[0,_t6,_t7,_t8,_t9,_ta,_tf]];}}else{return [0,_iD,new T(function(){return _te!=_t5?[0,_t0,_t1,_t2,_t3,_te,_t5]:E(_tc);}),[0,_t6,_t7,_t8,_t9,_ta,_tf]];}};return new F(function(){return _td(_t4,_tb,_);});},_tl=function(_tm,_tn,_){var _to=E(_tm),_tp=E(_tn);return new F(function(){return _sZ(_to[1],_to[2],_to[3],_to[4],_to[5],_to[6],_tp[1],_tp[2],_tp[3],_tp[4],_tp[5],_tp[6],_);});},_tq=new T(function(){return B(unCStr("UTF-32LE"));}),_tr=function(_ts){return [0,_tq,function(_){return [0,_rZ,function(_sG,_sH,_){return new F(function(){return _mh(_ts,_sG,_sH,_);});},_sx,_sx,_sU];},function(_){return [0,_tl,function(_sG,_sH,_){return new F(function(){return _mI(_ts,_sG,_sH,_);});},_sx,_sx,_sU];}];},_tt=function(_tu,_tv,_tw,_tx,_ty,_tz,_tA,_tB,_tC,_tD,_tE,_tF,_){var _tG=[0,_tu,_tv,_tw,_tx,0,0],_tH=function(_tI,_tJ,_){while(1){var _tK=(function(_tL,_tM,_){if(_tM<_tD){if(_tL<_tz){var _tN=readOffAddr("w32",4,_tu,_tL),_tO=_tN,_=0,_tP=_tO;if(_tP>127){if(_tP>2047){if(_tP>65535){if((_tD-_tM|0)>=4){var _=writeOffAddr("w8",1,plusAddr(_tA,_tM),0,((_tP>>18)+240|0)>>>0&255),_=0,_tQ=63>>>0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+1|0),0,(((_tP>>12>>>0&_tQ)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+2|0),0,(((_tP>>6>>>0&_tQ)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+3|0),0,(((_tP>>>0&_tQ)>>>0&4294967295)+128|0)>>>0&255),_=0,_tR=_tL+1|0,_tS=_tM+4|0;_tI=_tR;_tJ=_tS;return null;}else{return [0,_iE,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}}else{var _tT=function(_tU){var _tV=function(_tW){if((_tD-_tM|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_tA,_tM),0,((_tP>>12)+224|0)>>>0&255),_=0,_tX=63>>>0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+1|0),0,(((_tP>>6>>>0&_tX)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+2|0),0,(((_tP>>>0&_tX)>>>0&4294967295)+128|0)>>>0&255),_=0;return new F(function(){return _tH(_tL+1|0,_tM+3|0,_);});}else{return [0,_iE,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}};if(56320>_tP){return new F(function(){return _tV(_);});}else{return _tP>57343?B(_tV(_)):[0,_mV,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}};if(55296>_tP){return new F(function(){return _tT(_);});}else{return _tP>56319?B(_tT(_)):[0,_mV,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}}}else{if((_tD-_tM|0)>=2){var _=writeOffAddr("w8",1,plusAddr(_tA,_tM),0,((_tP>>6)+192|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_tA,_tM+1|0),0,(((_tP>>>0&63>>>0)>>>0&4294967295)+128|0)>>>0&255),_=0,_tR=_tL+1|0,_tS=_tM+2|0;_tI=_tR;_tJ=_tS;return null;}else{return [0,_iE,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}}}else{var _=writeOffAddr("w8",1,plusAddr(_tA,_tM),0,_tP>>>0&255),_=0,_tR=_tL+1|0,_tS=_tM+1|0;_tI=_tR;_tJ=_tS;return null;}}else{return [0,_iD,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}}else{return [0,_iE,new T(function(){return _tL!=_tz?[0,_tu,_tv,_tw,_tx,_tL,_tz]:E(_tG);}),[0,_tA,_tB,_tC,_tD,_tE,_tM]];}})(_tI,_tJ,_);if(_tK!=null){return _tK;}}};return new F(function(){return _tH(_ty,_tF,_);});},_tY=function(_tZ,_u0,_){var _u1=E(_tZ),_u2=E(_u0);return new F(function(){return _tt(_u1[1],_u1[2],_u1[3],_u1[4],_u1[5],_u1[6],_u2[1],_u2[2],_u2[3],_u2[4],_u2[5],_u2[6],_);});},_u3=function(_u4,_){return _Y;},_u5=function(_){return _Y;},_u6=function(_u7,_u8,_u9,_ua,_ub,_uc,_ud,_ue,_uf,_ug,_uh,_ui,_){var _uj=[0,_u7,_u8,_u9,_ua,0,0],_uk=function(_ul,_um,_){while(1){var _un=(function(_uo,_up,_){if(_up<_ug){if(_uo<_uc){var _uq=readOffAddr("w8",1,plusAddr(_u7,_uo),0),_ur=_uq,_=0;if(_ur>127){var _us=function(_ut){var _uu=function(_uv){if(_ur<240){return [0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}else{switch(_uc-_uo|0){case 1:return [0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];case 2:var _uw=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_ux=_uw,_=0,_uy=function(_uz){var _uA=function(_uB){return E(_ur)==244?_ux<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_ux>143?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];};if(_ur<241){return new F(function(){return _uA(_);});}else{if(_ur>243){return new F(function(){return _uA(_);});}else{if(_ux<128){return new F(function(){return _uA(_);});}else{return _ux>191?B(_uA(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}}};if(E(_ur)==240){if(_ux<144){return new F(function(){return _uy(_);});}else{return _ux>191?B(_uy(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return new F(function(){return _uy(_);});}break;case 3:var _uC=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_uD=_uC,_=0,_uE=readOffAddr("w8",1,plusAddr(_u7,_uo+2|0),0),_uF=_uE,_=0,_uG=function(_uH){var _uI=function(_uJ){return E(_ur)==244?_uD<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uD>143?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uF<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uF>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];};if(_ur<241){return new F(function(){return _uI(_);});}else{if(_ur>243){return new F(function(){return _uI(_);});}else{if(_uD<128){return new F(function(){return _uI(_);});}else{if(_uD>191){return new F(function(){return _uI(_);});}else{if(_uF<128){return new F(function(){return _uI(_);});}else{return _uF>191?B(_uI(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}}}}};if(E(_ur)==240){if(_uD<144){return new F(function(){return _uG(_);});}else{if(_uD>191){return new F(function(){return _uG(_);});}else{if(_uF<128){return new F(function(){return _uG(_);});}else{return _uF>191?B(_uG(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}}}else{return new F(function(){return _uG(_);});}break;default:var _uK=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_uL=_uK,_=0,_uM=readOffAddr("w8",1,plusAddr(_u7,_uo+2|0),0),_uN=_uM,_=0,_uO=readOffAddr("w8",1,plusAddr(_u7,_uo+3|0),0),_uP=_uO,_=0,_uQ=function(_uR){var _=writeOffAddr("w32",4,_ud,_up,(((((_ur&4294967295)-240|0)<<18)+(((_uL&4294967295)-128|0)<<12)|0)+(((_uN&4294967295)-128|0)<<6)|0)+((_uP&4294967295)-128|0)|0),_=0;return new F(function(){return _uk(_uo+4|0,_up+1|0,_);});},_uS=function(_uT){var _uU=function(_uV){return E(_ur)==244?_uL<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uL>143?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uN<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uN>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uP<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uP>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:B(_uQ(_)):[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];};if(_ur<241){return new F(function(){return _uU(_);});}else{if(_ur>243){return new F(function(){return _uU(_);});}else{if(_uL<128){return new F(function(){return _uU(_);});}else{if(_uL>191){return new F(function(){return _uU(_);});}else{if(_uN<128){return new F(function(){return _uU(_);});}else{if(_uN>191){return new F(function(){return _uU(_);});}else{if(_uP<128){return new F(function(){return _uU(_);});}else{return _uP>191?B(_uU(_)):B(_uQ(_));}}}}}}}};if(E(_ur)==240){if(_uL<144){return new F(function(){return _uS(_);});}else{if(_uL>191){return new F(function(){return _uS(_);});}else{if(_uN<128){return new F(function(){return _uS(_);});}else{if(_uN>191){return new F(function(){return _uS(_);});}else{if(_uP<128){return new F(function(){return _uS(_);});}else{return _uP>191?B(_uS(_)):B(_uQ(_));}}}}}}else{return new F(function(){return _uS(_);});}}}};if(_ur<224){return new F(function(){return _uu(_);});}else{if(_ur>239){return new F(function(){return _uu(_);});}else{switch(_uc-_uo|0){case 1:return [0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];case 2:var _uW=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_uX=_uW,_=0,_uY=function(_uZ){var _v0=function(_v1){var _v2=function(_v3){return _ur<238?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uX<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_uX>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];};if(E(_ur)==237){if(_uX<128){return new F(function(){return _v2(_);});}else{return _uX>159?B(_v2(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return new F(function(){return _v2(_);});}};if(_ur<225){return new F(function(){return _v0(_);});}else{if(_ur>236){return new F(function(){return _v0(_);});}else{if(_uX<128){return new F(function(){return _v0(_);});}else{return _uX>191?B(_v0(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}}};if(E(_ur)==224){if(_uX<160){return new F(function(){return _uY(_);});}else{return _uX>191?B(_uY(_)):[0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return new F(function(){return _uY(_);});}break;default:var _v4=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_v5=_v4,_=0,_v6=readOffAddr("w8",1,plusAddr(_u7,_uo+2|0),0),_v7=_v6,_=0,_v8=function(_v9){var _=writeOffAddr("w32",4,_ud,_up,((((_ur&4294967295)-224|0)<<12)+(((_v5&4294967295)-128|0)<<6)|0)+((_v7&4294967295)-128|0)|0),_=0;return new F(function(){return _uk(_uo+3|0,_up+1|0,_);});},_va=function(_vb){var _vc=function(_vd){var _ve=function(_vf){return _ur<238?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_v5<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_v5>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_v7<128?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:_v7>191?[0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]]:B(_v8(_));};if(E(_ur)==237){if(_v5<128){return new F(function(){return _ve(_);});}else{if(_v5>159){return new F(function(){return _ve(_);});}else{if(_v7<128){return new F(function(){return _ve(_);});}else{return _v7>191?B(_ve(_)):B(_v8(_));}}}}else{return new F(function(){return _ve(_);});}};if(_ur<225){return new F(function(){return _vc(_);});}else{if(_ur>236){return new F(function(){return _vc(_);});}else{if(_v5<128){return new F(function(){return _vc(_);});}else{if(_v5>191){return new F(function(){return _vc(_);});}else{if(_v7<128){return new F(function(){return _vc(_);});}else{return _v7>191?B(_vc(_)):B(_v8(_));}}}}}};if(E(_ur)==224){if(_v5<160){return new F(function(){return _va(_);});}else{if(_v5>191){return new F(function(){return _va(_);});}else{if(_v7<128){return new F(function(){return _va(_);});}else{return _v7>191?B(_va(_)):B(_v8(_));}}}}else{return new F(function(){return _va(_);});}}}}};if(_ur<192){return new F(function(){return _us(_);});}else{if(_ur>223){return new F(function(){return _us(_);});}else{if((_uc-_uo|0)>=2){var _vg=readOffAddr("w8",1,plusAddr(_u7,_uo+1|0),0),_vh=_vg,_=0;if(_vh>=128){if(_vh<192){var _=writeOffAddr("w32",4,_ud,_up,(((_ur&4294967295)-192|0)<<6)+((_vh&4294967295)-128|0)|0),_=0,_vi=_uo+2|0,_vj=_up+1|0;_ul=_vi;_um=_vj;return null;}else{return [0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return [0,_mV,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return [0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}}}else{var _=writeOffAddr("w32",4,_ud,_up,_ur&4294967295),_=0,_vi=_uo+1|0,_vj=_up+1|0;_ul=_vi;_um=_vj;return null;}}else{return [0,_iD,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}}else{return [0,_iE,new T(function(){return _uo!=_uc?[0,_u7,_u8,_u9,_ua,_uo,_uc]:E(_uj);}),[0,_ud,_ue,_uf,_ug,_uh,_up]];}})(_ul,_um,_);if(_un!=null){return _un;}}};return new F(function(){return _uk(_ub,_ui,_);});},_vk=function(_vl,_vm,_){var _vn=E(_vl),_vo=E(_vm);return new F(function(){return _u6(_vn[1],_vn[2],_vn[3],_vn[4],_vn[5],_vn[6],_vo[1],_vo[2],_vo[3],_vo[4],_vo[5],_vo[6],_);});},_vp=new T(function(){return B(unCStr("UTF-8"));}),_vq=function(_vr){return [0,_vp,function(_){return [0,_vk,function(_vs,_vt,_){return new F(function(){return _mh(_vr,_vs,_vt,_);});},_u5,_u5,_u3];},function(_){return [0,_tY,function(_vs,_vt,_){return new F(function(){return _mI(_vr,_vs,_vt,_);});},_u5,_u5,_u3];}];},_vu=function(_vv,_vw,_){var _vx=B(_hg(_vw));return !B(_c4(_vx,_h7))?!B(_c4(_vx,_h6))?!B(_c4(_vx,_h5))?!B(_c4(_vx,_hb))?!B(_c4(_vx,_ha))?!B(_c4(_vx,_h9))?!B(_c4(_vx,_h8))?B(_mO(_vv,_vw,_)):new T(function(){return B(_vq(_vv));}):new T(function(){return B(_tr(_vv));}):new T(function(){return B(_sX(_vv));}):new T(function(){return B(_sz(_vv));}):new T(function(){return B(_qc(_vv));}):new T(function(){return B(_pB(_vv));}):new T(function(){return B(_pd(_vv));});},_vy=function(_vz,_){var _vA=B((function(_vB,_){while(1){var _vC=readOffAddr("i8",1,_vz,_vB),_vD=_vC;if(!E(_vD)){return [0,_vB];}else{var _vE=_vB+1|0;_vB=_vE;continue;}}})(0,_)),_vF=_vA,_vG=E(_vF)[1];if(_vG>0){return new F(function(){return (function(_vH,_vI,_){while(1){var _vJ=readOffAddr("i8",1,_vz,_vI),_vK=_vJ;if(_vI>0){var _vL=[1,[0,_vK>>>0&255&4294967295],_vH],_vM=_vI-1|0;_vH=_vL;_vI=_vM;continue;}else{return [1,[0,_vK>>>0&255&4294967295],_vH];}}})(_C,_vG-1|0,_);});}else{return _C;}},_vN=function(_){var _=0,_vO=localeEncoding(),_vP=_vO;return new F(function(){return _vy(_vP,_);});},_vQ=new T(function(){return B(_7(_vN));}),_vR=function(_){var _=0;return new F(function(){return _vu(_h4,_vQ,_);});},_vS=new T(function(){return B(_7(_vR));}),_vT=function(_){var _=0,_vU=nMV(_vS),_vV=_vU;return [0,function(_){return new F(function(){return rMV(_vV);});},function(_vW,_){var _=wMV(_vV,_vW);return _Y;}];},_iy=new T(function(){return B(_7(_vT));}),_vX=new T(function(){return B(unCStr("no threads to run:  infinite loop or deadlock?"));}),_vY=new T(function(){return B(unCStr("%s"));}),_vZ=function(_w0,_){var _w1=E(_w0);return _Y;},_w2=function(_w3){return E(E(_w3)[1]);},_w4=function(_w5,_){var _w6=E(_w5),_w7=_w6[1],_w8=_w6[2],_w9=jsCatch(_eH,_vZ),_wa=_w9,_wb=E(_iy)[1],_wc=B(A(_wb,[_])),_wd=_wc;return new F(function(){return _gB(_wd,_vY,function(_we,_){var _wf=B(A(_wb,[_])),_wg=_wf;return new F(function(){return _gB(_wg,new T(function(){var _wh=B(A(_58,[_w7,_])),_wi=_wh[1],_wj=_wh[2],_wk=E(_en),_wl=hs_eqWord64(_wi,_wk[1]),_wm=_wl,_wn=function(_wo){var _wp=E(_ev),_wq=hs_eqWord64(_wi,_wp[1]),_wr=_wq;if(!E(_wr)){return new F(function(){return A(_w2,[B(_eB(_w7)),_eA,_w8,_C]);});}else{var _ws=hs_eqWord64(_wj,_wp[2]),_wt=_ws;if(!E(_wt)){return new F(function(){return A(_w2,[B(_eB(_w7)),_eA,_w8,_C]);});}else{return E(_w8);}}};if(!E(_wm)){var _wu=B(_wn(_));}else{var _wv=hs_eqWord64(_wj,_wk[2]),_ww=_wv;if(!E(_ww)){var _wx=B(_wn(_));}else{var _wy=E(_w8),_wx=E(_vX);}var _wu=_wx;}var _wz=_wu,_wA=_wz;return _wA;}),function(_wB,_){var _wC=errorBelch2(E(_we)[1],E(_wB)[1]);return _Y;},_);});},_);});},_wD=function(_){var _=0,_wE=nMV(_w4),_wF=_wE;return [0,_wF];},_wG=new T(function(){return B(_7(_wD));}),_wH=function(_wI,_){return new F(function(){return jsCatch(new T(function(){var _wJ=E(_wI),_wK=_wJ[2],_wL=B(A(_58,[_wJ[1],_])),_wM=_wL[1],_wN=_wL[2],_wO=E(_ea),_wP=hs_eqWord64(_wM,_wO[1]),_wQ=_wP,_wR=function(_){var _wS=E(_eg),_wT=hs_eqWord64(_wM,_wS[1]),_wU=_wT,_wV=function(_){var _wW=E(_e4),_wX=_wW[1],_wY=_wW[2],_wZ=hs_eqWord64(_wM,_wX),_x0=_wZ,_x1=function(_){var _x2=hs_eqWord64(_wM,_wX),_x3=_x2;if(!E(_x3)){var _x4=rMV(E(_wG)[1]),_x5=_x4;return new F(function(){return A(_x5,[_wJ,_]);});}else{var _x6=hs_eqWord64(_wN,_wY),_x7=_x6;if(!E(_x7)){var _x8=rMV(E(_wG)[1]),_x9=_x8;return new F(function(){return A(_x9,[_wJ,_]);});}else{if(!E(_wK)){var _xa=stackOverflow();return _Y;}else{var _xb=rMV(E(_wG)[1]),_xc=_xb;return new F(function(){return A(_xc,[_wJ,_]);});}}}};if(!E(_x0)){return new F(function(){return _x1(_);});}else{var _xd=hs_eqWord64(_wN,_wY),_xe=_xd;return E(_xe)==0?B(_x1(_)):E(_wK)==2?_Y:B(_x1(_));}};if(!E(_wU)){return new F(function(){return _wV(_);});}else{var _xf=hs_eqWord64(_wN,_wS[2]),_xg=_xf;if(!E(_xg)){return new F(function(){return _wV(_);});}else{var _xh=E(_wK);return _Y;}}};if(!E(_wQ)){var _xi=E(_wR);}else{var _xj=hs_eqWord64(_wN,_wO[2]),_xk=_xj;if(!E(_xk)){var _xl=E(_wR);}else{var _xm=E(_wK),_xl=E(_el);}var _xi=_xl;}var _xn=_xi,_xo=_xn,_xp=_xo;return _xp;}),_xq);});},_xq=function(_xr,_){return new F(function(){return _wH(_xr,_);});},_xs=new T(function(){return B(unCStr("Prelude.undefined"));}),_xt=new T(function(){return B(err(_xs));}),_xu=new T(function(){return E(_xt);}),_xv=function(_){return new F(function(){return jsCatch(_xu,_xq);});},_xw=function(_){var _xx=newMVar(),_xy=_xx,_xz=die("Unsupported PrimOp: fork#"),_xA=_xz,_xB=B(_7U(_dU,_8M,_8J,_xt,_)),_xC=_xB;return new F(function(){return A(E(_xC)[1],[_]);});},_xD=function(_){return new F(function(){return _xw(_);});};
var hasteMain = function() {B(A(_xD, [0]));};window.onload = hasteMain;