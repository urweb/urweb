// Detect browser quirks that we should be aware of.

function needsDynPrefix() {
    var span = document.createElement("span");
    span.innerHTML = "<script>alert('test');</script>";
    var scripts = span.getElementsByTagName("script");
    return scripts.length == 0;
}

// Codepoint implementations brought from https://norbertlindenberg.com/2012/05/ecmascript-supplementary-characters/#String
if (!String.fromCodePoint) {
    String.fromCodePoint = function () {
	var chars = [], i;
	for(i = 0; i < arguments.length; ++i) {
	    var c = Number(arguments[i]);
	    if (!isFinite(c) || c < 0 || c > 0x10FFFF || Math.floor(c) !== c) {
		throw new RangeError("Invalid code point " + c);
	    }
	    if (c < 0x10000) {
		chars.push(c);
	    } else {
		c -= 0x10000;
		chars.push((c >> 10) + 0xD800);
		chars.push((c % 0x400) + 0xDC00);
	    }
	}
	return String.fromCharCode.apply(undefined, chars);
    };

    String.prototype.codePointAt = function (index) {
	var str = String(this);
	if (index < 0 || index >= str.length) {
	    return undefined;
	}
	var first = str.charCodeAt(index);
	if (first >= 0xD800 && first <= 0xDBFF && str.length > index + 1) {
	    var second = str.charCodeAt(index + 1);
	    if (second >= 0xDC00 && second <= 0xDFFF) {
		return ((first - 0xD800) << 10) + (second - 0xDC00) + 0x10000;
	    }
	}
	return first;
    };
}

function iterateString(s, fn) {
    var strIdx = 0, idx = 0, res, cp;
    for (; strIdx < s.length ;) {
	cp = s.codePointAt(strIdx);
	if (fn) {	    
	    res = fn(String.fromCodePoint(cp), idx, strIdx);
	    if (res === false) return;
	}
	strIdx += cp > 0xFFFF ? 2 : 1;
	++idx;
    }
}

function strSplit(s) {
    var chars = [];
    iterateString(s, function(c) { chars.push(c); });
    return chars;
}

var dynPrefix = needsDynPrefix() ? "<span style=\"display:none\">A</span>" : "";

// Function versions of operators

function not(x) { return !x; }
function neg(x) { return -x; }

function eq(x, y) { return x == y; }
function plus(x, y) { return x + y; }
function minus(x, y) { return x - y; }
function times(x, y) { return x * y; }
function div(x, y) { return x / y; }
function divInt(x, y) { if (y == 0) er("Division by zero"); var n = x / y; return n < 0 ? Math.ceil(n) : Math.floor(n); }
function mod(x, y) { return x % y; }
function modInt(x, y) { if (y == 0) er("Division by zero"); var n = x % y; return n < 0 ? Math.ceil(n) : Math.floor(n); }
function lt(x, y) { return x < y; }
function le(x, y) { return x <= y; }

// Characters

function ord(c) { return c.codePointAt(0); }

var isLowerBitm = [];
var a = isLowerBitm;
a[3] = 0x7FFFFFE; a[5] = 0x4200400; a[6] = 0x80000000; a[7] = 0xFF7FFFFF; a[8] = 0xAAAAAAAA; 
a[9] = 0x55AAAAAA; a[10] = 0xAAAAAB55; a[11] = 0xD4AAAAAA; a[12] = 0x4E243129; a[13] = 0xE6512D2A; 
a[14] = 0xB5555240; a[15] = 0xAA29AAAA; a[16] = 0xAAAAAAAA; a[17] = 0x93FAAAAA; a[18] = 0xFFFFAA85; 
a[19] = 0xFFFFFFFF; a[20] = 0xFFEFFFFF; a[21] = 0x1FFFFFF; a[22] = 0x3; a[23] = 0x1F; 
a[26] = 0x20; a[27] = 0x3C8A0000; a[28] = 0x10000; a[29] = 0xFFFFF000; a[30] = 0xAAE37FFF; 
a[31] = 0x192FAAAA; a[33] = 0xFFFF0000; a[34] = 0xFFFFFFFF; a[35] = 0xAAAAAAAA; a[36] = 0xAAAAA802; 
a[37] = 0xAAAAAAAA; a[38] = 0xAAAAD554; a[39] = 0xAAAAAAAA; a[40] = 0xAAAAAAAA; a[41] = 0xAAAA; 
a[43] = 0xFFFFFFFE; a[44] = 0xFF; a[159] = 0x3F000000; a[228] = 0x1FF; a[232] = 0xFFFFFFFF; 
a[233] = 0xFFFFFFFF; a[234] = 0xFFFFFFFF; a[235] = 0xFFFFFFFF; a[236] = 0xFFFFFFFF; a[237] = 0xFFFFFFFF; 
a[240] = 0xAAAAAAAA; a[241] = 0xAAAAAAAA; a[242] = 0xAAAAAAAA; a[243] = 0xAAAAAAAA; a[244] = 0xBFEAAAAA; 
a[245] = 0xAAAAAAAA; a[246] = 0xAAAAAAAA; a[247] = 0xAAAAAAAA; a[248] = 0x3F00FF; a[249] = 0xFF00FF; 
a[250] = 0xFF003F; a[251] = 0x3FFF00FF; a[252] = 0xFF00FF; a[253] = 0x40DF00FF; a[254] = 0xCF00DC; 
a[255] = 0xDC00FF; a[259] = 0x80020000; a[260] = 0x1FFF0000; a[264] = 0x8C400; a[265] = 0x32108000; 
a[266] = 0x43C0; a[267] = 0xFFFF0000; a[268] = 0x10; a[294] = 0xFFFF0000; a[295] = 0x3FF; 
a[353] = 0xFFFF0000; a[354] = 0x7FFFFFFF; a[355] = 0x3FDA1562; a[356] = 0xAAAAAAAA; a[357] = 0xAAAAAAAA; 
a[358] = 0xAAAAAAAA; a[359] = 0x8501A; a[360] = 0xFFFFFFFF; a[361] = 0x20BF; a[1330] = 0xAAAAAAAA; 
a[1331] = 0x2AAA; a[1332] = 0x3AAAAAAA; a[1337] = 0xAAABAAA8; a[1338] = 0xAAAAAAAA; a[1339] = 0x95FFAAAA; 
a[1340] = 0xAABA50AA; a[1341] = 0xA002AA; a[1343] = 0x7000000; a[1369] = 0xFFFF0000; a[1370] = 0xF7FFFFFF; 
a[1371] = 0xFFFF003F; a[1372] = 0xFFFFFFFF; a[1373] = 0xFFFFFFFF; a[2008] = 0xF8007F; a[2042] = 0x7FFFFFE; 
a[2081] = 0xFFFFFF00; a[2082] = 0xFFFF; a[2086] = 0xFF000000; a[2087] = 0xFFFFFFF; a[2150] = 0xFFFFFFFF; 
a[2151] = 0x7FFFF; a[2246] = 0xFFFFFFFF; a[3744] = 0xFC000000; a[3745] = 0xFFFFF; a[3746] = 0xFFDFC000; 
a[3747] = 0xFF; a[3748] = 0xFFFFFFC; a[3749] = 0xEBC00000; a[3750] = 0xFFEF; a[3751] = 0xFFFFFC00; 
a[3752] = 0xC000000F; a[3753] = 0xFFFFFF; a[3754] = 0xFFFC0000; a[3755] = 0xFFF; a[3756] = 0xFFFFFFC0; 
a[3757] = 0xFC000000; a[3758] = 0xFFFFF; a[3759] = 0xFFFFC000; a[3760] = 0xFF; a[3761] = 0xFFFFFFC; 
a[3762] = 0xFFC00000; a[3763] = 0xFFFF; a[3764] = 0xFFFFFC00; a[3765] = 0x3F; a[3766] = 0xF7FFFFFC; 
a[3767] = 0xF0000003; a[3768] = 0xFDFFFFF; a[3769] = 0xFFC00000; a[3770] = 0x3F7FFF; a[3771] = 0xFFFF0000; 
a[3772] = 0xFDFF; a[3773] = 0xFFFFFC00; a[3774] = 0xBF7; a[3913] = 0xFFFFFFFC; a[3914] = 0xF; 

delete a;

function isLower(c) {
    var cp = ord(c);
    var idx = Math.floor(cp / 32);
    var byt = isLowerBitm[idx];
    if (byt)
    {
	var mask = Math.pow(2, cp - idx * 32);
	return (byt & mask) != 0;
    }
    return false;
}

var isUpperBitm = [];
var a = isUpperBitm;
a[2] = 0x7FFFFFE; a[6] = 0x7F7FFFFF; a[8] = 0x55555555; a[9] = 0xAA555555; a[10] = 0x555554AA; 
a[11] = 0x2B555555; a[12] = 0xB1DBCED6; a[13] = 0x11AED2D5; a[14] = 0x4AAAA490; a[15] = 0x55D25555; 
a[16] = 0x55555555; a[17] = 0x6C055555; a[18] = 0x557A; a[27] = 0x80450000; a[28] = 0xFFFED740; 
a[29] = 0xFFB; a[30] = 0x551C8000; a[31] = 0xE6905555; a[32] = 0xFFFFFFFF; a[33] = 0xFFFF; 
a[35] = 0x55555555; a[36] = 0x55555401; a[37] = 0x55555555; a[38] = 0x55552AAB; a[39] = 0x55555555; 
a[40] = 0x55555555; a[41] = 0xFFFE5555; a[42] = 0x7FFFFF; a[133] = 0xFFFFFFFF; a[134] = 0x20BF; 
a[157] = 0xFFFFFFFF; a[158] = 0xFFFFFFFF; a[159] = 0x3FFFFF; a[240] = 0x55555555; a[241] = 0x55555555; 
a[242] = 0x55555555; a[243] = 0x55555555; a[244] = 0x40155555; a[245] = 0x55555555; a[246] = 0x55555555; 
a[247] = 0x55555555; a[248] = 0x3F00FF00; a[249] = 0xFF00FF00; a[250] = 0xAA003F00; a[251] = 0xFF00; 
a[253] = 0xF000000; a[254] = 0xF000F00; a[255] = 0xF001F00; a[264] = 0x3E273884; a[265] = 0xC00F3D50; 
a[266] = 0x20; a[267] = 0xFFFF; a[268] = 0x8; a[293] = 0xFFC00000; a[294] = 0xFFFF; 
a[352] = 0xFFFFFFFF; a[353] = 0x7FFF; a[355] = 0xC025EA9D; a[356] = 0x55555555; a[357] = 0x55555555; 
a[358] = 0x55555555; a[359] = 0x42805; a[1330] = 0x55555555; a[1331] = 0x1555; a[1332] = 0x5555555; 
a[1337] = 0x55545554; a[1338] = 0x55555555; a[1339] = 0x6A005555; a[1340] = 0x55452855; a[1341] = 0x5F7D55; 
a[2041] = 0x7FFFFFE; a[2080] = 0xFFFFFFFF; a[2081] = 0xFF; a[2085] = 0xFFFF0000; a[2086] = 0xFFFFF; 
a[2148] = 0xFFFFFFFF; a[2149] = 0x7FFFF; a[2245] = 0xFFFFFFFF; a[3744] = 0x3FFFFFF; a[3745] = 0xFFF00000; 
a[3746] = 0x3FFF; a[3747] = 0xFFFFFF00; a[3748] = 0xD0000003; a[3749] = 0x3FDE64; a[3750] = 0xFFFF0000; 
a[3751] = 0x3FF; a[3752] = 0x1FDFE7B0; a[3753] = 0x7B000000; a[3754] = 0x1FC5F; a[3755] = 0xFFFFF000; 
a[3756] = 0x3F; a[3757] = 0x3FFFFFF; a[3758] = 0xFFF00000; a[3759] = 0x3FFF; a[3760] = 0xFFFFFF00; 
a[3761] = 0xF0000003; a[3762] = 0x3FFFFF; a[3763] = 0xFFFF0000; a[3764] = 0x3FF; a[3765] = 0xFFFFFF00; 
a[3766] = 0x1; a[3767] = 0x7FFFFFC; a[3768] = 0xF0000000; a[3769] = 0x1FFFFF; a[3770] = 0xFFC00000; 
a[3771] = 0x7FFF; a[3772] = 0xFFFF0000; a[3773] = 0x1FF; a[3774] = 0x400; a[3912] = 0xFFFFFFFF; 
a[3913] = 0x3; a[3977] = 0xFFFF0000; a[3978] = 0xFFFF03FF; a[3979] = 0xFFFF03FF; a[3980] = 0x3FF; 

delete a;

function isUpper(c) {
    var cp = ord(c);
    var idx = Math.floor(cp / 32);
    var byt = isUpperBitm[idx];
    if (byt) {
	var mask = Math.pow(2, cp - idx * 32);
	return (byt & mask) != 0;
    }
    return false;
}

var isAlphaBitm = (function(){
	var a = 0xFFFFFFFF;
	var b = 0xFFFF0000;
	var c = 0x7FFFFFFF;
	var d = 0x3FFFFF;
	var e = 0x3FFFFFFF;
	var f = 0x1FFFFFFF;
	var g = 0x7FFFFF;
	var h = 0x7FFFF;
	var i = 0x7FFFFFE;
	var j = 0xFFFFFFFE;
	var k = 0x1FFFFFF;
	var l = 0x7FFFFFF;
	var m = 0xF7FFFFFF;
	var n = 0xFFFFF;
	var o = 0xFFFFFFF;
	var p = 0x3FFFF;
	var q = 0xE3EDFDFF;
	var r = 0xFFFDDFEF;
	var s = 0xFFFF07FF;
	var t = 0xFFFF7FFF;
	var u = 0x3FFFFFF;
	var v = 0xFFFFFDFF;
	var w = 0xFFDFFFFF;
	var x = 0xFF7FFFFF;
	var y = 0xFFFFE000;
	var z = 0xFFFFFC00;
	var a0 = 0xFFF99FEF;
	var a1 = 0xFFFFFEFF;
	var a2 = 0xFFFF20BF;
	var a3 = 0x3F3FFFFF;
	var a4 = 0x7F7F7F7F;
	var a5 = 0xFF800000;
	var a6 = 0xFFFFFFFC;
	var a7 = 0xFFF80000;
	var a8 = 0xFFFCFFFF;
	var a9 = 0x1FFFFF;
	var b0 = 0xFFFFFFEF;
	var b1 = 0xFFFF03FF;
	return [
	0, 0, i, i, 0, 0x4200400, x, x, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, 0x3FFC3, 0x501F, 0, 0, 0x20, 0xBCDF0000, 0xFFFFD740, 0xFFFFFFFB, a, 0xFFBFFFFF, a, a, a, a, 0xFFFFFC03, a, a, a, 
	a, 0xFFFEFFFF, 0x27FFFFF, j, 0xFF, 0xBFFF0000, 0xFFFF00B6, 0x707FF, 0x7FF0000, a, 0xFEFFFFFF, 0xFFFFC000, a, a, 0x1FEFFFFF, 0x9C00E1FE, b, a, y, a, 
	a, p, z, 0x43007FF, 0xFCFFFFFF, 0x1FFF, k, 0x7FF, 0, 0x3FDFFFFF, 0xFFF00000, 0xFFFF03F8, a, 0xEFFFFFFF, 0xFFE1DFFF, 0xFFFE000F, a0, 0xE3C5FDFF, 0xB080599F, 0x1003000F, 
	0xFFF987EE, 0xC36DFDFF, 0x5E021987, 0x3F0000, 0xFFFBBFEE, q, 0x11BBF, 0x1E00000F, 0xFFF99FEE, q, 0xB0C0199F, 0x2000F, 0xD63DC7EC, 0xC3FFC718, 0x811DC7, 0, r, 0xE3FFFDFF, 0x7601DDF, 0xF, 
	r, 0xE3EFFDFF, 0x40601DDF, 0x6000F, r, 0xE7FFFFFF, 0x80F05DDF, 0xFC00000F, 0xFC7FFFEC, 0x2FFBFFFF, 0xFF5F807F, 0xC0000, j, l, 0x207F, 0, 0xFEF02596, 0x3BFFECAE, 0xF000205F, 0, 
	0x1, 0, a1, 0xFFFE1FFF, 0xFEFFFF03, f, 0, 0, a, 0xF97FFFFF, b, 0xFFFFC1E7, 0x3000407F, a, a2, m, a, a, a, a, 
	a, a, a, a, a, a, 0x3D7F3DFF, a, 0xFFFF3DFF, 0x7F3DFFFF, 0xFF7FFF3D, a, 0xFF3DFFFF, a, 0x87FFFFFF, 0, 0xFFFF, a, a, a3, 
	j, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0xFFFF9FFF, 
	i, a, a, 0x1FFC7FF, 0xFDFFF, n, n, 0xDDFFF, a, 0xFFCFFFFF, 0x108001FF, 0, 0, a, a, 0xFFFFFF, a, s, a, d, 
	c, 0x1FF0FFF, b, 0x1F3FFF, a, 0xFFFF0FFF, 0x3FF, 0, o, a, c, 0x1FFFFE, 0, 0x80, 0, 0, a, 0xFFEFFFFF, 0xFEF, 0, 
	a, 0xFC00F3FF, a, 0x3FFBF, a, d, 0xFC00E000, e, 0x1FF, 0, 0, 0x6FDE00, a, a, a, a, a, a, 0, 0x1FFF80, 
	a, a, a, a, a, a, a, a, a3, a, 0xAAFF3F3F, e, a, 0x5FDFFFFF, 0xFCF1FDC, 0x1FDC1FFF, 0, 0, 0, 0x80020000, 
	0x1FFF0000, 0, 0, 0, 0x3E2FFC84, 0xF3FFBD50, 0x43E0, a, 0x1FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xFFC00000, a, 0x3FF, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, t, c, a, a, a, a, 0xC781F, 
	a, a2, a, 0x80FF, g, a4, a4, a, 0, 0x8000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0xE0, 0x1F3E03FE, j, a, 0xE07FFFFF, j, a, m, 0xFFFFFFE0, 0xFFFE7FFF, a, a, 0x7FFF, l, 0, b, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, d, 0, 0, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0x7FF, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0x1FFF, 0, b, e, 
	a, a, a, a, a, a, a, a, 0xFFFF1FFF, 0xC00, a, 0x8FF07FFF, a, a, a, 0xFFFF, a5, a6, a, a, 
	0xFFFFF9FF, 0xFF7FFF, 0, a5, 0xFFFFF7BB, 0xFF, a, n, a, a, 0x2F, 0x28FC0000, z, s, h, f, a, 0xFFF7FFFF, 0x8000, 0x7C00FFDF, 
	a, g, 0x3FFF, 0xC47FFFFF, a, c, 0x38000005, 0x3CFFFF, 0x7E7E7E, 0xFFFF7F7F, m, 0xFFFF003F, a, a, a, 0x7FF, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, 0xFFFF000F, 0xFFFFF87F, o, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, 0xFFFF3FFF, a, a, u, 0, 0xE0F8007F, 0x5F7FFDFF, 0xFFFFFFDB, a, a, p, a7, a, a, a, a, a, 
	a, a, a, a, a, e, b, a, a8, a, 0xFF, 0xFFF0000, 0, 0, 0, 0xFFDF0000, a, a, a, f, 
	0, i, i, 0xFFFFFFC0, a, c, 0x1CFCFCFC, 0, 0xFFFFEFFF, 0xB7FFFF7F, 0x3FFF3FFF, 0, a, a, a, l, 0, 0, a, a9, 
	0, 0, 0, 0, 0, 0, 0, 0, f, a, 0x1FFFF, 0, a, y, s, l, e, a, 0x3EFF0F, 0, 
	a, a, a, a, e, b, 0xFF0FFFFF, o, a, 0xFFFF00FF, a, 0xF, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, a, g, d, 0xFF, 0, 0, 0, 0, 0xFFFFFD3F, 0x91BFFFFF, d, g, c, 0, 0, 0x37FFFF, 
	d, u, 0, 0, a, 0xC0FFFFFF, 0, 0, 0xFEEFF06F, n, 0, f, f, 0, a1, 0x1F, a, d, d, h, 
	p, 0, 0, 0, a, a, 0x1FF, 0, a, h, a, h, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0x3F, 0, 
	a6, k, b, 0x1FF, a, h, b, 0x47FFFF, a, a, 0x1400001E, 0, 0xFFFBFFFF, 0x409FFFFF, 0, 0, 0xBFFFBD7F, 0xFFFF01FF, a, 0x1FF, 
	a0, q, 0xE081199F, 0xF, 0, 0, 0, 0, a, a, 0x7BB, 0, a, a, 0xB3, 0, 0, 0, 0, 0, 
	a, 0x7F3FFFFF, 0x3F000000, 0, a, c, 0x11, 0, a, d, 0, 0, 0xE3FFFFFF, 0x7FF, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, a, a, 0x80000000, 0, 0, 0, 0, 0, 0, 0, 0, a, 0x7FE7FFFF, b, a, 
	0xFFFFCF, 0, a, k, 0, 0, 0, 0, 0, 0, 0, 0, v, 0x7F7FFFFF, 0x1, 0xFFFC0000, a8, 0x7FFEFF, 0, 0, 
	0xFFFFFB7F, 0xB47FFFFF, 0xCB, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, u, 0, 0, 0, a, a, a, 0x7FFF, 
	a, a, a, a, a, a, 0xF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, 0x7FFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, 0x7F, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, k, c, 0, 
	0, 0, b, 0x3FFF, a, g, 0xF, 0xE0FFFFF8, 0xFFFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0xFFFF001F, c, 
	a7, 0, 0, 0x3, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0x1FFF, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, h, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, c, 0, 0, b, a, a, a, a, a, a, a, a, a, a, a, o, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0x1FFF07FF, 0x43FF01FF, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, a, a, w, a, 0xDFFFFFFF, 0xEBFFDE64, b0, a, 0xDFDFE7BF, 0x7BFFFFFF, 0xFFFDFC5F, a, a, a, a, a, 
	a, a, a, a, a, 0xFFFFFF3F, 0xF7FFFFFD, m, w, w, t, t, v, v, 0xFF7, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0xF9FFFF7F, 0x7DB, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, a, a, a, a, a, a, 0x1F, 0, a, a, 0x8F, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b0, 0xAF7FE96, 0xAA96EA84, 0x5EF7F796, 0xFFFFBFF, 0xFFFFBEE, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b, b1, b1, 
	0x3FF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, g, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a9, a, a, a, a, a, a, e, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, 0xFFFF0003, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, 0x1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, e
	];})();

function isAlpha(c) {
    var cp = ord(c);
    var idx = Math.floor(cp / 32);
    var byt = isAlphaBitm[idx];
    if (byt) {
	var mask = Math.pow(2, cp - idx * 32);
	return (byt & mask) != 0;
    }
    return false;
}

var isDigitBitm = [];
var a = isDigitBitm;
a[1] = 0x3FF0000; a[51] = 0x3FF; a[55] = 0x3FF0000; a[62] = 0x3FF; 
a[75] = 0xFFC0; a[79] = 0xFFC0; a[83] = 0xFFC0; a[87] = 0xFFC0; 
a[91] = 0xFFC0; a[95] = 0xFFC0; a[99] = 0xFFC0; a[103] = 0xFFC0; 
a[107] = 0xFFC0; a[111] = 0xFFC0; a[114] = 0x3FF0000; a[118] = 0x3FF0000; 
a[121] = 0x3FF; a[130] = 0x3FF; a[132] = 0x3FF0000; a[191] = 0x3FF; 
a[192] = 0x3FF0000; a[202] = 0xFFC0; a[206] = 0x3FF0000; a[212] = 0x3FF03FF; 
a[218] = 0x3FF0000; a[221] = 0x3FF0000; a[226] = 0x3FF03FF; a[1329] = 0x3FF; 
a[1350] = 0x3FF0000; a[1352] = 0x3FF; a[1358] = 0x3FF0000; a[1359] = 0x3FF0000; 
a[1362] = 0x3FF0000; a[1375] = 0x3FF0000; a[2040] = 0x3FF0000; a[2085] = 0x3FF; 
a[2179] = 0xFFC0; a[2183] = 0x3FF0000; a[2185] = 0xFFC00000; a[2190] = 0x3FF0000; 
a[2199] = 0x3FF0000; a[2210] = 0x3FF0000; a[2214] = 0x3FF0000; a[2226] = 0x3FF0000; 
a[2230] = 0x3FF; a[2233] = 0x3FF0000; a[2247] = 0x3FF; a[2274] = 0x3FF0000; 
a[2282] = 0x3FF0000; a[2899] = 0x3FF; a[2906] = 0x3FF0000; a[3774] = 0xFFFFC000; 
a[3775] = 0xFFFFFFFF; a[3914] = 0x3FF0000; 
delete a;

function isDigit(c) {
    var cp = ord(c);
    var idx = Math.floor(cp / 32);
    var byt = isDigitBitm[idx];
    if (byt) {
	var mask = Math.pow(2, cp - idx * 32);
	return (byt & mask) != 0;
    }
    return false;
}

function isAlnum(c) { return isAlpha(c) || isDigit(c); }
function isBlank(c) {
    var cp = ord(c);
    if (cp == 9)
	return true;
    if (cp == 32)
	return true;
    if (cp == 160)
	return true;
    if (cp == 5760)
	return true;
    if (cp >= 8192 && cp <= 8202)
	return true;
    if (cp == 8239)
	return true;
    if (cp == 8287)
	return true;
    if (cp == 12288)
	return true;
    
    return false;
}
function isSpace(c) {
    var cp = ord(c);
    if (cp >= 10 && cp <= 13)
	return true;
    if (cp == 133)
	return true;
    if (cp == 8232)
	return true;
    if (cp == 8233)
	return true;
    
    return isBlank(c);
}
function isXdigit(c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }

var isPrintBitm = (function(){
	var a = 0xFFFFFFFF;
	var b = 0x7FFFFFFF;
	var c = 0x7FFFFF;
	var d = 0x3FFFFFFF;
	var e = 0xFFFF0FFF;
	var f = 0xFFFFFFF;
	var g = 0x3FFFFF;
	var h = 0xFFFF00FF;
	var i = 0x1FFFFFFF;
	var j = 0x3FFFFFF;
	var k = 0xFFFFFF;
	var l = 0xFFFF0000;
	var m = 0xFFFFFFFE;
	var n = 0x7FFFFFF;
	var o = 0xFFFFF;
	var p = 0xFFFF1FFF;
	var q = 0xDFFFFFFF;
	var r = 0xF3EDFDFF;
	var s = 0xFFFDDFEF;
	var t = 0x1FFFFFF;
	var u = 0xFFDFFFFF;
	var v = 0xFF3FFFFF;
	var w = 0xFFFF7FFF;
	var x = 0xFFFF000F;
	var y = 0xFFFCFFFF;
	var z = 0x3FF00FF;
	var a0 = 0x1FFFFF;
	var a1 = 0xFE7FFFFF;
	var a2 = 0x3FFFF;
	var a3 = 0x7FFF3FFF;
	var a4 = 0xFFF99FEF;
	var a5 = 0xFFFF20BF;
	var a6 = 0x3F3FFFFF;
	var a7 = 0x3FF03FF;
	var a8 = 0xFFFF07FF;
	var a9 = 0x9FFFFFFF;
	var b0 = 0xFBFFFFFF;
	var b1 = 0x1FFFF;
	var b2 = 0xE3FFFFFF;
	var b3 = 0x7F7F7F7F;
	var b4 = 0xFFFE7FFF;
	var b5 = 0xFFFF003F;
	var b6 = 0xFFFF3FFF;
	var b7 = 0xFFFF03FF;
	var b8 = 0x7FFFF;
	var b9 = 0xFFFF0003;
	var c0 = 0xFFFFFFEF;
	return [
	0, a, a, b, 0, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, 0xFCFFFFFF, 0xFFFFD7F0, 0xFFFFFFFB, a, a, a, a, a, a, a, a, a, a, 
	a, 0xFFFEFFFF, a1, m, 0xFFFEE6FF, a, h, 0x1F07FF, q, a, a, a, a, a, a, a, 0xFFFFBFFF, a, 0xFFFFE7FF, a, 
	a, a2, a, n, a, a3, 0x4FFFFFFF, 0x7FF, 0, 0x3FDFFFFF, 0xFFF00000, a, a, a, a, a, a4, 0xF3C5FDFF, 0xB080799F, 0x3FFFFFCF, 
	0xFFF987EE, 0xD36DFDFF, 0x5E023987, 0x3FFFC0, 0xFFFBBFEE, r, 0x13BBF, 0xFE03FFCF, 0xFFF99FEE, r, 0xB0C0399F, 0xFFFFCF, 0xD63DC7EC, 0xC3FFC718, 0x813DC7, 0x7FFFFC0, s, 0xE3FFFDFF, 0x7603DDF, 0xFF00FFCF, 
	s, 0xF3EFFDFF, 0x40603DDF, 0x6FFCF, s, a, 0xFFF0FDDF, 0xFFFFFFCF, 0xFC7FFFEC, 0x2FFBFFFF, 0xFF5F847F, 0x1CFFC0, m, 0x87FFFFFF, f, 0, 0xFEF02596, 0x3BFFECAE, 0xF3FF3F5F, 0, 
	a, a, 0xFFFFFEFF, 0xFFFE1FFF, 0xFEFFFFFF, q, 0x7FFDFFF, 0, a, a, a, a, a, a, a5, a, a, a, a, a, 
	a, a, a, a, a, a, 0x3D7F3DFF, a, 0xFFFF3DFF, 0x7F3DFFFF, 0xFF7FFF3D, a, 0xFF3DFFFF, a, 0xE7FFFFFF, i, j, a, a, a6, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	i, a, a, t, 0x1FDFFF, c, o, 0xDDFFF, a, a, d, a7, 0x3FF7FFF, a, a, k, a, a8, a, g, 
	b, 0xFFF0FFF, 0xFFFFFFF1, 0x1F3FFF, a, e, 0xC7FF03FF, a, 0xCFFFFFFF, a, b, a9, a7, a3, 0, 0, a, a, e, i, 
	a, a, a, 0xF00FFFFF, a, 0xF8FFFFFF, 0xFFFFE3FF, a, 0x1FF, 0, h, j, a, a, a, a, a, a, a, b0, 
	a, a, a, a, a, a, a, a, a6, a, 0xAAFF3F3F, d, a, u, 0xEFCFFFDF, 0x7FDCFFFF, a, 0xFFFFFCFF, a, 0xFFF3FFDF, 
	0x1FFF7FFF, a, l, b1, a, a, a, a, e, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, 0x7F, 0x7FF, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, 0xFFCFFFFF, v, b2, 0x7FDFF, 0xF000, a, w, b, a, a, a, a, 0xFE0FFFFF, 
	a, a5, a, 0x800180FF, c, b3, b3, a, a, a, 0x3FF, 0, b0, a, a, o, a, a, a, a, 
	a, a, g, 0xFFF0000, a, a, m, a, a1, a, a, a, 0xFFFFFFE0, b4, a, a, w, n, a, x, 
	b, a, a, a, a, a, a, b, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, g, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0x7FF, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, p, a, 0xFFFF007F, a, 
	a, a, a, a, a, a, a, a, a, 0xFFF, a, a, a, a, a, k, a, a, a, a, 
	a, 0xFF7FFF, 0, 0xFF800000, a, 0x3FF0FFF, a, k, a, a, 0x3FFC03F, d, a, a, 0x800FFFFF, i, a, a, 0xC3FFBFFF, b, 
	a, c, 0xF3FF3FFF, a, a, a, 0xF8000007, c, 0x7E7E7E, 0xFFFF7F7F, a, b5, a, a, a, 0x3FF3FFF, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, x, 0xFFFFF87F, f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, b6, a, a, j, 0, 0xE0F8007F, 0x5F7FFFFF, 0xFFFFFFDB, a, a, a, 0xFFF80003, a, a, a, a, a, 
	a, a, a, a, a, a, l, a, y, a, 0xFF, 0x3FFF0000, j, a, 0xFFF7FFFF, 0xFFDF0F7F, a, a, a, a9, 
	m, a, a, a, a, b, 0x1CFCFCFC, 0x3E007F7F, 0xFFFFEFFF, 0xB7FFFF7F, 0x3FFF3FFF, 0, a, a, a, n, 0xFFFFFF87, 0xFF8FFFFF, a, a, 
	0xFFF7FFF, 0x1, l, d, 0, 0, 0, 0, i, a, b1, f, a, 0xFFFFE00F, a8, n, 0xBFFFFFFF, a, 0x3FFF0F, 0, 
	a, a, a, a, d, b7, 0xFF0FFFFF, f, a, h, a, 0x800F, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, a, c, g, 0xFF, 0, 0, 0, 0, 0xFFFFFD3F, 0x91BFFFFF, 0xFFBFFFFF, a, b, 0xFF80, 0, 0xF837FFFF, 
	0x8FFFFFFF, 0x83FFFFFF, 0, 0, a, 0xF0FFFFFF, y, a, 0xFEEFF06F, 0x870FFFFF, 0x1FF00FF, a, a, 0, a, 0x7FF87F, a, 0xFE3FFFFF, v, 0xFF07FFFF, 
	0x1E03FFFF, 0xFE00, 0, 0, a, a, 0x1FF, 0, a, b8, a, 0xFC07FFFF, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0xFFFC3FFF, 0x8000FFFF, 
	a, a, b9, 0x3FF01FF, a, u, x, c, a, a, b6, 0x1FFFFE, 0xFFFBFFFF, b, 0, 0, 0xBFFFBD7F, b7, a, 0x3FF07FF, 
	a4, r, 0xE081399F, 0x1F1FCF, 0, 0, 0, 0, a, a, 0x2BFFFFFF, 0, a, a, z, 0, 0, 0, 0, 0, 
	a, v, d, 0, a, a, 0x3FF001F, 0x1FFF, a, k, 0x3FF, 0, b2, e, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, a, a, 0x8007FFFF, 0, 0, 0, 0, 0, 0, 0, 0, a, a, h, a, 
	0xDFFFFFCF, 0x7, a, t, 0, 0, 0, 0, 0, 0, 0, 0, 0xFFFFFDFF, 0xFF7FFFFF, b5, p, y, 0x7FFEFF, 0, 0, 
	0xFFFFFB7F, 0xB47FFFFF, z, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, j, 0, 0, 0, a, a, a, 0x1F7FFF, 
	a, a, a, a, a, a, 0xF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, 0x7FFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, 0x7F, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, t, b, 0xC3FF, 
	0, 0, l, 0x3F3FFF, a, a, 0xFBFF003F, 0xE0FFFFFB, 0xFFFF, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, 0xFFFF001F, b, 
	0xFFFF8000, 0, 0, 0x3, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0x1FFF, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, b8, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, b, 0, 0, l, a, a, a, a, a, a, a, a, a, a, a, f, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, 0x1FFF07FF, 0xF3FF01FF, 0xF, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, a, a, a, g, 
	a, 0xFFFFFE7F, a, a, a, a, a, 0x1FF, a, a, 0x3F, 0, 0, 0, 0, 0, a, a, c, a2, 
	0, 0, 0, 0, a, a, u, a, q, 0xEBFFDE64, c0, a, 0xDFDFE7BF, 0x7BFFFFFF, 0xFFFDFC5F, a, a, a, a, a, 
	a, a, a, a, a, 0xFFFFFF3F, a, a, a, a, a, a, a, a, 0xFFFFCFFF, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 0xF8000FFF, 0xFFFE, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0xF9FFFF7F, 0x7DB, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, a, a, a, a, a, a, 0x7FFF9F, 0, a, a, 0xC3FF07FF, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c0, 0xAF7FE96, 0xAA96EA84, 0x5EF7F796, 0xFFFFBFF, 0xFFFFBEE, 0, 0x30000, 
	0, 0, 0, 0, 0, 0, 0, 0, a, e, a, a, o, b4, 0xFFFEFFFE, g, p, w, a, e, 
	a, 0x1FFF, 0, 0xFFFFFFC0, 0xFFFF0007, f, 0x301FF, 0x3F, 0, 0, 0, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a0, 0x1FF1FFF, a, a, a, o, a, a, a0, 0, e, a, z, a, h, 0x3FFF, 0, 0, 
	e, b, p, 0xFFF, k, 0, 0xFFFF0001, 0x7F, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, c, 0, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a0, a, a, a, a, a, a, d, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, b9, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, 
	a, a, a, 0x1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, d
	];})();

function isPrint(c) {
    var cp = ord(c);
    var idx = Math.floor(cp / 32);
    var byt = isPrintBitm[idx];
    if (byt) {
	var mask = Math.pow(2, cp - idx * 32);
	return (byt & mask) != 0;
    }
    return false;
}

function toLower(c) {
    var cp = ord(c);
    
    if (cp == 304)
	return chr(105);
    else if (cp >= 7312 && cp <= 7354)
	return c;
    else if (cp >= 7357 && cp <= 7359)
	return c;
    else if (cp == 42936)
	return c;
    else if (cp >= 93760 && cp <= 93791)
	return c;
    return c.toLowerCase();
}

function toUpper(c) {
    var cp = ord(c);
    if (cp == 223)
	return c;
    else if (cp == 329)
	return c;
    else if (cp == 496)
	return c;
    else if (cp == 912)
	return c;
    else if (cp == 944)
	return c;
    else if (cp == 1415)
	return c;
    else if (cp >= 4304 && cp <= 4346)
	return c;
    else if (cp >= 4349 && cp <= 4351)
	return c;
    else if (cp >= 7830 && cp <= 7834)
	return c;
    else if (cp == 8016)
	return c;
    else if (cp == 8018)
	return c;
    else if (cp == 8020)
	return c;
    else if (cp == 8022)
	return c;
    else if (cp >= 8064 && cp <= 8071)
	return chr(cp + 8)
    else if (cp >= 8072 && cp <= 8079)
	return c;
    else if (cp >= 8080 && cp <= 8087)
	return chr(cp + 8);
    else if (cp >= 8088 && cp <= 8095)
	return c;
    else if (cp >= 8096 && cp <= 8103)
	return chr(cp + 8)
    else if (cp >= 8104 && cp <= 8111)
	return c;
    else if (cp == 8114)
	return c;
    else if (cp == 8115)
	return chr(8124);
    else if (cp == 8116)
	return c;
    else if (cp == 8118)
	return c;
    else if (cp == 8119)
	return c;
    else if (cp == 8124)
	return c;
    else if (cp == 8130)
	return c;
    else if (cp == 8131)
	return chr(8140);
    else if (cp == 8132)
	return c;
    else if (cp == 8134)
	return c;
    else if (cp == 8135)
	return c;
    else if (cp == 8140)
	return c;
    else if (cp == 8146)
	return c;
    else if (cp == 8147)
	return c;
    else if (cp == 8150)
	return c;
    else if (cp == 8151)
	return c;
    else if (cp >= 8162 && cp <= 8164)
	return c;
    else if (cp == 8166)
	return c;
    else if (cp == 8167)
	return c;
    else if (cp == 8178)
	return c;
    else if (cp == 8179)
	return chr(8188);
    else if (cp == 8180)
	return c;
    else if (cp == 8182)
	return c;
    else if (cp == 8183)
	return c;
    else if (cp == 8188)
	return c;
    else if (cp == 42937)
	return c;
    else if (cp >= 64256 && cp <= 64262)
	return c;
    else if (cp >= 64275 && cp <= 64279)
	return c;
    else if (cp >= 93792 && cp <= 93823)
	return c;
    else
	return c.toUpperCase();
}

// Lists

function cons(v, ls) {
    return { next : ls, data : v };
}
function rev(ls) {
    var acc = null;
    for (; ls; ls = ls.next)
	acc = cons(ls.data, acc);
    return acc;
}
function concat(ls1, ls2) {
    var acc = ls2;
    ls1 = rev(ls1);
    for (; ls1; ls1 = ls1.next)
        acc = cons(ls1.data, acc);
    return acc;
}
function member(x, ls) {
    for (; ls; ls = ls.next)
        if (ls.data == x)
            return true;
    return false;
}
function remove(x, ls) {
    var acc = null;

    for (; ls; ls = ls.next)
        if (ls.data == x)
            return concat(acc, ls.next);
    else
        acc = cons(ls.data, acc);

    return ls;
}
function union(ls1, ls2) {
    var acc = ls2;

    for (; ls1; ls1 = ls1.next)
        if (!member(ls1.data, ls2))
            acc = cons(ls1.data, acc);

    return acc;
}
function length(ls) {
    var acc = 0;

    for (; ls; ls = ls.next)
        ++acc;

    return acc;
}


// Floats

function float(n) {
    return n;
}

function trunc(n) {
    return ~~n;
}

function ceil(n) {
    return Math.ceil(n);
}

function round(n) {
    return Math.round(n);
}

function pow(n, m) {
    return Math.pow(n, m);
}

function sqrt(n){
    return Math.sqrt(n);
}

function sin(n){
    return Math.sin(n);
}

function cos(n){
    return Math.cos(n);
}

function log(n){
    return Math.log(n);
}

function exp(n){
    return Math.exp(n);
}

function asin(n){
    return Math.asin(n);
}
function acos(n){
    return Math.acos(n);
}

function atan(n){
    return Math.atan(n);
}

function atan2(n, m){
    return Math.atan2(n, m);
}

function floor(n){
    return Math.floor(n);
}

function abs(n){
    return Math.abs(n);
}

// Time, represented as counts of microseconds since the epoch

var time_format = "%c";

function showTime(tm) {
    return strftime(time_format, tm);
}

function showTimeHtml(tm) {
    return eh(showTime(tm));
}

function now() {
    return (new Date()).getTime() * 1000;
}

function diffInSeconds(tm1, tm2) {
    return Math.round((tm2 - tm1) / 1000000);
}

function diffInMilliseconds(tm1, tm2) {
    return Math.round((tm2 - tm1) / 1000);
}

function toSeconds(tm) {
    return Math.round(tm / 1000000);
}

function toMilliseconds(tm) {
    return Math.round(tm / 1000);
}

function fromMilliseconds(tm) {
    return tm * 1000;
}

function addSeconds(tm, n) {
    return tm + n * 1000000;
}

function stringToTime_error(string) {
    var t = Date.parse(string);
    if (isNaN(t))
        er("Invalid date string: " + string);
    else
        return t * 1000;
}

function stringToTime(string) {
    try {
        var t = Date.parse(string);
        if (isNaN(t))
            return null;
        else
            return t * 1000;
    } catch (e) {
        return null;
    }
}

/*
  strftime() implementation from:
  YUI 3.4.1 (build 4118)
  Copyright 2011 Yahoo! Inc. All rights reserved.
  Licensed under the BSD License.
  http://yuilibrary.com/license/
*/

var xPad=function (x, pad, r)
{
    if(typeof r === "undefined")
    {
	r=10;
    }
    pad = pad.toString();
    for( ; parseInt(x, 10)<r && r>1; r/=10) {
	x = pad + x;
    }
    return x.toString();
};

var YDateEn = {
    a: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    A: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
    b: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    B: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    c: "%a %d %b %Y %T %Z",
    p: ["AM", "PM"],
    P: ["am", "pm"],
    r: "%I:%M:%S %p",
    x: "%d/%m/%y",
    X: "%T"
};

var Dt = {
    formats: {
	a: function (d, l) { return l.a[d.getDay()]; },
	A: function (d, l) { return l.A[d.getDay()]; },
	b: function (d, l) { return l.b[d.getMonth()]; },
	B: function (d, l) { return l.B[d.getMonth()]; },
	C: function (d) { return xPad(parseInt(d.getFullYear()/100, 10), 0); },
	d: ["getDate", "0"],
	e: ["getDate", " "],
	g: function (d) { return xPad(parseInt(Dt.formats.G(d)%100, 10), 0); },
	G: function (d) {
	    var y = d.getFullYear();
	    var V = parseInt(Dt.formats.V(d), 10);
	    var W = parseInt(Dt.formats.W(d), 10);

	    if(W > V) {
		y++;
	    } else if(W===0 && V>=52) {
		y--;
	    }

	    return y;
	},
	H: ["getHours", "0"],
	I: function (d) { var I=d.getHours()%12; return xPad(I===0?12:I, 0); },
	j: function (d) {
	    var gmd_1 = new Date("" + d.getFullYear() + "/1/1 GMT");
	    var gmdate = new Date("" + d.getFullYear() + "/" + (d.getMonth()+1) + "/" + d.getDate() + " GMT");
	    var ms = gmdate - gmd_1;
	    var doy = parseInt(ms/60000/60/24, 10)+1;
	    return xPad(doy, 0, 100);
	},
	k: ["getHours", " "],
	l: function (d) { var I=d.getHours()%12; return xPad(I===0?12:I, " "); },
	m: function (d) { return xPad(d.getMonth()+1, 0); },
	M: ["getMinutes", "0"],
	p: function (d, l) { return l.p[d.getHours() >= 12 ? 1 : 0 ]; },
	P: function (d, l) { return l.P[d.getHours() >= 12 ? 1 : 0 ]; },
	s: function (d, l) { return parseInt(d.getTime()/1000, 10); },
	S: ["getSeconds", "0"],
	u: function (d) { var dow = d.getDay(); return dow===0?7:dow; },
	U: function (d) {
	    var doy = parseInt(Dt.formats.j(d), 10);
	    var rdow = 6-d.getDay();
	    var woy = parseInt((doy+rdow)/7, 10);
	    return xPad(woy, 0);
	},
	V: function (d) {
	    var woy = parseInt(Dt.formats.W(d), 10);
	    var dow1_1 = (new Date("" + d.getFullYear() + "/1/1")).getDay();
	    var idow = woy + (dow1_1 > 4 || dow1_1 <= 1 ? 0 : 1);
	    if(idow === 53 && (new Date("" + d.getFullYear() + "/12/31")).getDay() < 4)
	    {
		idow = 1;
	    }
	    else if(idow === 0)
	    {
		idow = Dt.formats.V(new Date("" + (d.getFullYear()-1) + "/12/31"));
	    }

	    return xPad(idow, 0);
	},
	w: "getDay",
	W: function (d) {
	    var doy = parseInt(Dt.formats.j(d), 10);
	    var rdow = 7-Dt.formats.u(d);
	    var woy = parseInt((doy+rdow)/7, 10);
	    return xPad(woy, 0, 10);
	},
	y: function (d) { return xPad(d.getFullYear()%100, 0); },
	Y: "getFullYear",
	z: function (d) {
	    var o = d.getTimezoneOffset();
	    var H = xPad(parseInt(Math.abs(o/60), 10), 0);
	    var M = xPad(Math.abs(o%60), 0);
	    return (o>0?"-":"+") + H + M;
	},
	Z: function (d) {
	    var tz = d.toString().replace(/^.*:\d\d( GMT[+-]\d+)? \(?([A-Za-z ]+)\)?\d*$/, "$2").replace(/[a-z ]/g, "");
	    if(tz.length > 4) {
		tz = Dt.formats.z(d);
	    }
	    return tz;
	},
	"%": function (d) { return "%"; }
    },

    aggregates: {
	c: "locale",
	D: "%m/%d/%y",
	F: "%Y-%m-%d",
	h: "%b",
	n: "\n",
	r: "%I:%M:%S %p",
	R: "%H:%M",
	t: "\t",
	T: "%H:%M:%S",
	x: "locale",
	X: "locale"
    },

    format : function (oDate, format) {
	var replace_aggs = function (m0, m1) {
	    var f = Dt.aggregates[m1];
	    return (f === "locale" ? YDateEn[m1] : f);
	};

	var replace_formats = function (m0, m1) {
	    var f = Dt.formats[m1];
	    switch(typeof f) {
	    case "string":
		return oDate[f]();
	    case "function":
		return f.call(oDate, oDate, YDateEn);
	    case "array":
            case "object":
		if(typeof(f[0]) === "string")
		    return xPad(oDate[f[0]](), f[1]);
	    default:
		return m1;
	    }
	};

	while(format.match(/%[cDFhnrRtTxX]/)) {
	    format = format.replace(/%([cDFhnrRtTxX])/g, replace_aggs);
	}

	var str = format.replace(/%([aAbBCdegGHIjklmMpPsSuUVwWyYzZ%])/g, replace_formats);

	replace_aggs = replace_formats = undefined;

	return str;
    }
};

// End of YUI code

function strftime(fmt, thisTime)
{
    var thisDate = new Date();
    thisDate.setTime(Math.floor(thisTime / 1000));
    return Dt.format(thisDate, fmt);
};

function fromDatetime(year, month, date, hour, minute, second) {
  return (new Date(year, month, date, hour, minute, second)).getTime() * 1000;
};

function datetimeYear(t) {
  return (new Date(t / 1000)).getYear() + 1900;
};

function datetimeMonth(t) {
  return (new Date(t / 1000)).getMonth();
};

function datetimeDay(t) {
  return (new Date(t / 1000)).getDate();
};

function datetimeHour(t) {
  return (new Date(t / 1000)).getHours();
};

function datetimeMinute(t) {
  return (new Date(t / 1000)).getMinutes();
};

function datetimeSecond(t) {
  return (new Date(t / 1000)).getSeconds();
};

function datetimeDayOfWeek(t) {
  return (new Date(t / 1000)).getDay();
};


// Error handling

function uw_debug(msg) {
    try {
        console.debug(msg);
    } catch (e) {
        alert("DEBUG: " + msg);
    }

    return 0;
}

var lameDuck = false;

function suppressErrors() {
    lameDuck = true;
}

function whine(msg) {
    if (lameDuck) return;
    alert(msg);
    throw msg;
}

function pf(loc) {
    throw ("Pattern match failure (" + loc + ")");
}

function runHandlers(kind, ls, arg) {
    if (!lameDuck) {
        if (ls == null)
            alert(kind + ": " + arg);
        for (; ls; ls = ls.next)
            try {
                exec({c:"a", f:{c:"a", f:ls.data, x:{c:"c", v:arg}}, x:{c:"c", v:null}});
            } catch (v) { }
    }
}

var errorHandlers = null;

function flift0(v) {
    return {c:"c", v:v};
}

function onError(f) {
    errorHandlers = cons(flift0(f), errorHandlers);
}

function er(s) {
    runHandlers("Error", errorHandlers, s);
    throw {uw_error: s};
}

var failHandlers = null;

function onFail(f) {
    failHandlers = cons(flift0(f), failHandlers);
}

function doExn(v) {
    if (v == null || v.uw_error == null) {
        var s = (v == null ? "null" : v.message ? v.message : v.toString());
        if (v != null && v.fileName && v.lineNumber)
            s += " (" + v.fileName + ":" + v.lineNumber + ")";
        runHandlers("Fail", failHandlers, s);
    }
}

var disconnectHandlers = null;

function flift(f) {
    return {c: "c", v:{env:cons(f,null), body:{c:"v", n:1}}};
}

function onDisconnect(f) {
    disconnectHandlers = cons(flift(f), disconnectHandlers);
}

function discon() {
    runHandlers("Disconnect", disconnectHandlers, null);
}

var connectHandlers = null;

function onConnectFail(f) {
    connectHandlers = cons(flift(f), connectHandlers);
}

function conn(msg) {
    var rx = /(.*)<body>((.|\n|\r)*)<\/body>(.*)/g;
    var arr = rx.exec(msg);
    msg = (arr && arr.length >= 3) ? arr[2] : msg;
    runHandlers("RPC failure", connectHandlers, msg);
}

var serverHandlers = null;

function onServerError(f) {
    serverHandlers = cons(flift0(f), serverHandlers);
}

function servErr(s) {
    window.setTimeout(function () { runHandlers("Server", serverHandlers, s); }, 0);
}

// Key and mouse events

var uw_event = null;

function uw_getEvent() {
    return window.event ? window.event : uw_event;
}

function firstGood(x, y) {
    if (x == undefined || x == 0)
        return y;
    else
        return x;
}

function uw_mouseEvent() {
    var ev = uw_getEvent();

    return {_ScreenX : firstGood(ev.screenX, 0),
            _ScreenY : firstGood(ev.screenY, 0),
            _ClientX : firstGood(ev.clientX, 0),
            _ClientY : firstGood(ev.clientY, 0),
            _OffsetX : firstGood(ev.offsetX, 0),
            _OffsetY : firstGood(ev.offsetY, 0),
            _CtrlKey : firstGood(ev.ctrlKey, false),
            _ShiftKey : firstGood(ev.shiftKey, false),
            _AltKey : firstGood(ev.altKey, false),
            _MetaKey : firstGood(ev.metaKey, false),
            _Button : ev.button == 2 ? "Right" : ev.button == 1 ? "Middle" : "Left"};
}

function uw_keyEvent() {
    var ev = uw_getEvent();

    return {_KeyCode : firstGood(ev.keyCode, ev.which),
            _CtrlKey : firstGood(ev.ctrlKey, false),
            _ShiftKey : firstGood(ev.shiftKey, false),
            _AltKey : firstGood(ev.altKey, false),
            _MetaKey : firstGood(ev.metaKey, false)};
}



// Document events

function uw_handler(name, f) {
    var old = document[name];
    if (old == undefined)
        document[name] = function(event) { uw_event = event; execF(execF(f, uw_mouseEvent())); };
    else
        document[name] = function(event) { uw_event = event; old(); execF(execF(f, uw_mouseEvent())); };
}

function uw_onClick(f) {
    uw_handler("onclick", f);
}

function uw_onContextmenu(f) {
    uw_handler("oncontextmenu", f);
}

function uw_onDblclick(f) {
    uw_handler("ondblclick", f);
}

function uw_onMousedown(f) {
    uw_handler("onmousedown", f);
}

function uw_onMouseenter(f) {
    uw_handler("onmouseenter", f);
}

function uw_onMouseleave(f) {
    uw_handler("onmouseleave", f);
}

function uw_onMousemove(f) {
    uw_handler("onmousemove", f);
}

function uw_onMouseout(f) {
    uw_handler("onmouseout", f);
}

function uw_onMouseover(f) {
    uw_handler("onmouseover", f);
}

function uw_onMouseup(f) {
    uw_handler("onmouseup", f);
}

function uw_keyHandler(name, f) {
    var old = document[name];
    if (old == undefined)
        document[name] = function(event) { uw_event = event; execF(execF(f, uw_keyEvent())); };
    else
        document[name] = function(event) { uw_event = event; old(); execF(execF(f, uw_keyEvent())); };
}

function uw_onKeydown(f) {
    uw_keyHandler("onkeydown", f);
}

function uw_onKeypress(f) {
    uw_keyHandler("onkeypress", f);
}

function uw_onKeyup(f) {
    uw_keyHandler("onkeyup", f);
}

// Cancelling of further event processing

function uw_preventDefault() {
    var e = window.event ? window.event : uw_event;
    e.returnValue = false;
    if (e.preventDefault) e.preventDefault();
}

function uw_stopPropagation() {
    var e = window.event ? window.event : uw_event;
    e.cancelBubble = true;
    if (e.stopPropagation) e.stopPropagation();
}

// Embedding closures in XML strings

function cs(f) {
    return {closure: f};
}

function isWeird(v) {
    return v.closure != null || v.cat1 != null;
}

function cat(s1, s2) {
    if (isWeird(s1) || isWeird(s2))
        return {cat1: s1, cat2: s2};
    else
        return s1 + s2;
}

var closures = [];
var freeClosures = null;

function newClosure(f) {
    var n;
    if (freeClosures == null) {
        n = closures.length;
    } else {
        n = freeClosures.data;
        freeClosures = freeClosures.next;
    }
    closures[n] = f;
    return n;
}

function freeClosure(n) {
    closures[n] = null;
    freeClosures = cons(n, freeClosures);
}

function cr(n) {
    return closures[n];
}

function flattenAcc(a, cls, trs) {
    while (trs) {
        var tr = trs.data;
        trs = trs.next;

        if (tr) {
            if (tr.cat1 != null) {
                trs = cons(tr.cat1, cons(tr.cat2, trs));
            } else if (tr.closure != null) {
                var cl = newClosure(tr.closure);
                cls.v = cons(cl, cls.v);
                a.push("cr(", cl.toString(), ")");
            } else
                a.push(tr);
        }
    }
}

function flatten(cls, tr) {
    var a = [];
    flattenAcc(a, cls, cons(tr, null));
    return a.join("");
}

function flattenLocal(s) {
    var cls = {v : null};
    var r = flatten(cls, s);
    for (cl = cls.v; cl != null; cl = cl.next)
        freeClosure(cl.data);
    return r;
}


// Dynamic tree management

function populate(node) {
    if (node.dead) return;

    var s = node.signal;
    var oldSources = node.sources;
    try {
        var sr = execF(s, null);
        var newSources = sr._sources;

        for (var sp = oldSources; sp; sp = sp.next)
            if (!member(sp.data, newSources))
                sp.data.dyns = remove(node, sp.data.dyns);

        for (var sp = newSources; sp; sp = sp.next)
            if (!member(sp.data, oldSources))
                sp.data.dyns = cons(node, sp.data.dyns);

        node.sources = newSources;
        node.recreate(sr._data);
    } catch (v) {
        doExn(v);
    }
}

function sc(v) {
    return {data : v, dyns : null};
}
function sv(s, v) {
    if (s.data != v) {
        s.data = v;

        for (var ls = s.dyns; ls; ls = ls.next)
            populate(ls.data);
    }
}
function sg(s) {
    return s.data;
}

function ss(s) {
    return {env:cons(s, null), body:{c:"r", l:
            cons({n:"sources", v:{c:"c", v:cons(s, null)}},
                 cons({n:"data", v:{c:"f", f:sg, a:cons({c:"v", n:1}, null)}}, null))}};
}
function sr(v) {
    return {env:null, body:{c:"c", v:{_sources : null, _data : v}}};
}
function sb(x,y) {
    return {env:cons(y,cons(x,null)),
            body:{c:"=",
                e1:{c:"a", f:{c:"v", n:2}, x:{c:"c", v:null}},
                e2:{c:"=",
                    e1:{c:"a",
                        f:{c:"a", f:{c:"v", n:2}, x:{c:".", r:{c:"v", n:0}, f:"data"}},
                        x:{c:"c", v:null}},
                    e2:{c:"r", l:cons(
                                      {n:"sources", v:{c:"f", f:union, a:cons({c:".", r:{c:"v", n:1}, f:"sources"},
                                                                              cons({c:".", r:{c:"v", n:0}, f:"sources"}, null))}},
                                      cons({n:"data", v:{c:".", r:{c:"v", n:0}, f:"data"}}, null))}}}};
}
function scur(s) {
    return execF(s, null)._data;
}

function lastParent() {
    var pos = document.body;

    while (pos.lastChild && pos.lastChild.nodeType == 1)
        pos = pos.lastChild;

    pos = pos.parentNode;

    return pos;
}

var thisScript = null;

function addNode(node) {
    if (thisScript) {
        if (thisScript.parentNode)
            thisScript.parentNode.replaceChild(node, thisScript);
    } else
        lastParent().appendChild(node);
}

function runScripts(node) {
    if (node.tagName == "SCRIPT") {
        var savedScript = thisScript;
        thisScript = node;

        try {
            eval(thisScript.text);
        } catch (v) {
            doExn(v);
        }
        if (thisScript.parentNode)
            thisScript.parentNode.removeChild(thisScript);

        thisScript = savedScript;
    } else if (node.getElementsByTagName) {
        var savedScript = thisScript;

        var scripts = node.getElementsByTagName("script"), scriptsCopy = [];
        var len = scripts.length;
        for (var i = 0; i < len; ++i)
            scriptsCopy[i] = scripts[i];
        for (var i = 0; i < len; ++i) {
            thisScript = scriptsCopy[i];

            try {
                eval(thisScript.text);
            } catch (v) {
                doExn(v);
            }
            if (thisScript.parentNode)
                thisScript.parentNode.removeChild(thisScript);
        }

        thisScript = savedScript;
    }
}


// Dynamic tree entry points

function killScript(scr) {
    scr.dead = true;
    for (var ls = scr.sources; ls; ls = ls.next)
        ls.data.dyns = remove(scr, ls.data.dyns);
    for (var ls = scr.closures; ls; ls = ls.next)
        freeClosure(ls.data);
}

// Sometimes we wind up with tables that contain <script>s outside the single <tbody>.
// To avoid dealing with that case, we normalize by moving <script>s into <tbody>.
function normalizeTable(table) {
    var orig = table;

    var script, next;

    while (table && table.tagName != "TABLE")
        table = table.parentNode;

    for (var tbody = table.firstChild; tbody; tbody = tbody.nextSibling) {
        if (tbody.tagName == "TBODY") {
            var firstChild = tbody.firstChild;

            for (script = table.firstChild; script && script != tbody; script = next) {
                next = script.nextSibling;

                if (script.tagName === "SCRIPT") {
                    if (firstChild)
                        tbody.insertBefore(script, firstChild);
                    else
                        tbody.appendChild(script);
                }
            }

            return;
        }
    }

    var tbody = document.createElement("tbody");
    for (script = table.firstChild; script; script = next) {
        next = script.nextSibling;

        tbody.appendChild(script);
    }
    table.appendChild(tbody);
}

var suspendScripts = false;

function dyn(pnode, s) {
    if (suspendScripts)
        return;

    var x = document.createElement("script");
    x.dead = false;
    x.signal = s;
    x.sources = null;
    x.closures = null;

    var firstChild = null;

    x.recreate = function(v) {
        for (var ls = x.closures; ls; ls = ls.next)
            freeClosure(ls.data);

        var next;
        for (var child = firstChild; child && child != x; child = next) {
            next = child.nextSibling;

            killScript(child);
            if (child.getElementsByTagName) {
                var arr = child.getElementsByTagName("script");
                for (var i = 0; i < arr.length; ++i)
                    killScript(arr[i]);
            }

            if (child.parentNode)
                child.parentNode.removeChild(child);
        }

        var cls = {v : null};
        var html = flatten(cls, v);
        if (pnode != "table" && pnode != "tr")
            html = dynPrefix + html;
        x.closures = cls.v;

        if (pnode == "table") {
            normalizeTable(x.parentNode);

            var dummy = document.createElement("body");
            suspendScripts = true;
            try {
                dummy.innerHTML = "<table>" + html + "</table>";
            } catch (e) {
                suspendScripts = false;
                throw e;
            }

            var table = x.parentNode;

            if (table) {
                firstChild = null;
                var tbody;

                var arr = dummy.getElementsByTagName("tbody");

                var tbody;
                if (arr.length > 0 && arr[0].parentNode == dummy.firstChild) {
                    tbody = arr[0];
                    var next;
                    for (var node = dummy.firstChild.firstChild; node; node = next) {
                        next = node.nextSibling;

                        if (node.tagName != "TBODY")
                            tbody.appendChild(node);
                    }
                } else
                    tbody = dummy.firstChild;

                var next;
                firstChild = document.createElement("script");
                table.insertBefore(firstChild, x);
                for (var node = tbody.firstChild; node; node = next) {
                    next = node.nextSibling;
                    table.insertBefore(node, x);
                    suspendScripts = false;
                    runScripts(node);
                    suspendScripts = true;
                }
            }

            suspendScripts = false;
        } else if (pnode == "tr") {
            var dummy = document.createElement("body");
            suspendScripts = true;
            try {
                dummy.innerHTML = "<table><tr>" + html + "</tr></table>";
            } catch (e) {
                suspendScripts = false;
                throw e;
            }

            var table = x.parentNode;

            if (table) {
                var arr = dummy.getElementsByTagName("tr");
                firstChild = null;
                var tr;
                if (arr.length > 0 && table != null)
                    tr = arr[0];
                else
                    tr = dummy.firstChild;

                var next;
                firstChild = document.createElement("script");
                table.insertBefore(firstChild, x);
                for (var node = tr.firstChild; node; node = next) {
                    next = node.nextSibling;
                    table.insertBefore(node, x);
                    suspendScripts = false;
                    runScripts(node);
                    suspendScripts = true;
                }
            };

            suspendScripts = false;
        } else {
            firstChild = document.createElement("span");

            suspendScripts = true;
            try {
                firstChild.innerHTML = html;
                if (x.parentNode)
                    x.parentNode.insertBefore(firstChild, x);
            } catch (e) {
                suspendScripts = false;
                throw e;
            }
            suspendScripts = false;
            runScripts(firstChild);
        }
    };

    addNode(x);
    populate(x);
}

function setInnerHTML(node, html) {
    var x;

    if (node.previousSibling && node.previousSibling.closures != undefined) {
        x = node.previousSibling;

        for (var ls = x.closures; ls; ls = ls.next)
            freeClosure(ls.data);

        if (node.getElementsByTagName) {
            var arr = node.getElementsByTagName("script");
            for (var i = 0; i < arr.length; ++i)
                killScript(arr[i]);
        }
    } else {
        x = document.createElement("script");
        x.dead = false;
        x.sources = null;

        if (node.parentNode)
            node.parentNode.insertBefore(x, node);
        else
            whine("setInnerHTML: node is not already in the DOM tree");
    }

    var cls = {v : null};
    var html = flatten(cls, html);
    x.closures = cls.v;
    suspendScripts = true;
    node.innerHTML = html;
    suspendScripts = false;
    runScripts(node);
}

var maySuspend = true;

function active(s) {
    if (suspendScripts)
        return;

    var ms = maySuspend;
    maySuspend = false;
    try {
        var html = execF(s);
    } catch (e) {
        maySuspend = ms;
        throw e;
    }
    maySuspend = ms;
    if (html != "") {
        var span = document.createElement("span");
        addNode(span);
        setInnerHTML(span, html);
    }
}

function listen(s, onchange) {
    var x = document.createElement("script");
    x.dead = false;
    x.signal = s;
    x.sources = null;
    x.closures = null;
    x.recreate = onchange;
    populate(x);
}

function input(x, s, recreate, type, name) {
    if (name) x.name = name;
    if (type) x.type = type;
    addNode(x);

    var sc = document.createElement("script");
    sc.dead = false;
    sc.signal = ss(s);
    sc.sources = null;
    sc.recreate = recreate(x);

    if (x.parentNode)
        x.parentNode.insertBefore(sc, x);

    populate(sc);

    return x;
}

function inpt(type, s, name) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("input"), s,
                  function(x) { return function(v) { if (x.value != v) x.value = v; }; }, type, name);
    x.value = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.value) };

    return x;
}
function inpt_float(type, s, name) {
    if (suspendScripts)
        return;

    var filterFloat = function(value) {
	if (/^(\-|\+)?([0-9]+(\.[0-9]+)?|Infinity)$/
	    .test(value))
	    return Number(value);
	return null;
    }
    var x = input(document.createElement("input"), s, function(x) { return function(v) { if (x.value != v) x.value = v; }; }, type, name);
    x.value = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, filterFloat(x.value)) };

    return x;
}


function inp(s, name) {
    return inpt("text", s, name);
}

function password(s, name) {
    return inpt("password", s, name);
}

function email(s, name) {
    return inpt("email", s, name);
}

function search(s, name) {
    return inpt("search", s, name);
}

function url(s, name) {
    return inpt("url", s, name);
}

function tel(s, name) {
    return inpt("tel", s, name);
}

function color(s, name) {
    return inpt("color", s, name);
}

function number(s, name) {
    return inpt_float("number", s, name);
}

function range(s, name) {
    return inpt_float("range", s, name);
}

function date(s, name) {
    return inpt("date", s, name);
}

function datetime(s, name) {
    return inpt("datetime", s, name);
}

function datetime_local(s, name) {
    return inpt("datetime-local", s, name);
}

function month(s, name) {
    return inpt("month", s, name);
}

function week(s, name) {
    return inpt("week", s, name);
}

function time(s, name) {
    return inpt("time", s, name);
}

function crad(s) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("input"), s,
                  function(x) { return function(v) { x.checked = (x.value === v); }; }, "radio");
    x.onclick = x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.value) };
    setTimeout(function() {
	x.defaultChecked = x.checked = (s.data === x.value);
    }, 10);

    return x;
}

function selectValue(x) {
    if (x.options.length == 0)
        return "";
    else
        return x.options[x.selectedIndex].value;
}

function setSelectValue(x, v) {
  for (var i = 0; i < x.options.length; ++i) {
      if (x.options[i].value == v) {
          x.selectedIndex = i;
          return;
      }
  }
}

function sel(s, content) {
    if (suspendScripts)
        return;

    var dummy = document.createElement("span");
    dummy.innerHTML = "<select>" + content + "</select>";

    var x = dummy.firstChild;
    for (var i = 0; i < x.options.length; ++i) {
        if (x.options[i].value == "")
            x.options[i].value = x.options[i].text;
        else
            x.options[i].value = x.options[i].value.substring(1);
    }

    x = input(x, s, function(x) { return function(v) { if (selectValue(x) != v) setSelectValue(x, v); }; });

    setSelectValue(x, s.data);
    if (selectValue(x) != s.data)
        sv(s, selectValue(x));
    x.onchange = function() { sv(s, selectValue(x)) };

    return x;
}

function chk(s) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("input"), s,
                  function(x) { return function(v) { if (x.checked != v) x.checked = v; }; }, "checkbox");
    x.defaultChecked = x.checked = s.data;
    x.onclick = x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.checked) };

    return x;
}

function tbx(s) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("textarea"), s,
                  function(x) { return function(v) { if (x.value != v) x.value = v; }; });
    x.innerHTML = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.value) };

    return x;
}

function dynClass(pnode, html, s_class, s_style) {
    if (suspendScripts)
        return;

    var htmlCls = {v : null};
    html = flatten(htmlCls, html);
    htmlCls = htmlCls.v;

    var dummy = document.createElement(pnode);
    suspendScripts = true;
    dummy.innerHTML = html;
    suspendScripts = false;
    var html = dummy.firstChild;
    dummy.removeChild(html);
    if (pnode == "table" && html.tagName == "TBODY") {
        html = html.firstChild;
    }

    var x = null;
    var y = null;

    var classNameBefore = html.className;
    var styleCssBefore = html.style.cssText;

    if (s_class) {
        x = document.createElement("script");
        x.dead = false;
        x.signal = s_class;
        x.sources = null;
        x.closures = htmlCls;

        x.recreate = function(v) {
            for (var ls = x.closures; ls != htmlCls; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            var s = flatten(cls, v);
            if (classNameBefore)
                s += " " + classNameBefore;
            html.className = s;
	    x.closures = concat(cls.v, htmlCls);
        }

        populate(x);
    }

    if (s_style) {
        var htmlCls2 = s_class ? null : htmlCls;
        y = document.createElement("script");
        y.dead = false;
        y.signal = s_style;
        y.sources = null;
        y.closures = htmlCls2;

        y.recreate = function(v) {
            for (var ls = y.closures; ls != htmlCls2; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            var s = flatten(cls, v);
            if (styleCssBefore)
                s += " " + styleCssBefore;
            html.style.cssText = s;
	    y.closures = concat(cls.v, htmlCls2);
        }

        populate(y);
    }

    addNode(html);
    runScripts(html);

    if (x)
        html.appendChild(x);
    if (y)
        html.appendChild(y);
}

function bodyDynClass(s_class, s_style) {
    if (suspendScripts)
        return;

    var htmlCls = null;

    if (s_class) {
        var x = document.createElement("script");
        x.dead = false;
        x.signal = s_class;
        x.sources = null;
        x.closures = htmlCls;

        x.recreate = function(v) {
            for (var ls = x.closures; ls != htmlCls; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            document.body.className = flatten(cls, v);
            console.log("className to + " + document.body.className);
	    x.closures = concat(cls.v, htmlCls);
        }

        document.body.appendChild(x);
        populate(x);
    }

    if (s_style) {
        var htmlCls2 = s_class ? null : htmlCls;
        var y = document.createElement("script");
        y.dead = false;
        y.signal = s_style;
        y.sources = null;
        y.closures = htmlCls2;

        y.recreate = function(v) {
            for (var ls = y.closures; ls != htmlCls2; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            document.body.style.cssText = flatten(cls, v);
            console.log("style to + " + document.body.style.cssText);
	    y.closures = concat(cls.v, htmlCls2);
        }

        document.body.appendChild(y);
        populate(y);
    }
}

function addOnChange(x, f) {
    var old = x.onchange;
    if (old == null)
        x.onchange = f;
    else
        x.onchange = function() { old(); f(); };
}

function addOnInput(x, f) {
    var old = x.oninput;
    if (old == null)
        x.oninput = f;
    else
        x.oninput = function() { old(); f(); };
}

function addOnKeyUp(x, f) {
    var old = x.onkeyup;
    if (old == null)
        x.onkeyup = f;
    else
        x.onkeyup = function(x) { old(x); f(x); };
}


// Basic string operations

function eh(x) {
    if (x == null)
        return "NULL";
    else
        return flattenLocal(x).split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
}

function ts(x) { return x.toString() }
function bs(b) { return (b ? "True" : "False") }
function s2b(s) { return s == "True" ? true : s == "False" ? false : null; }
function s2be(s) { return s == "True" ? true : s == "False" ? false : er("Illegal Boolean " ^ s); }

function id(x) { return x; }
function subUtf8(s, i) {
    if (i < 0 || i >= s.length)
        er("String index " + i + " out of bounds");
    return s[i];
}
function sub(s, i) {
    var ch = undefined;
    iterateString(s, function(c, idx) { if (idx == i) { ch = c; return false; }});
    if (ch == undefined)
        er("String index " + i + " out of bounds")
    return ch;
}
function sufUtf8(s, i) {
    if (i < 0 || i > s.length)
        er("String index " + i + " out of bounds");
    return s.substring(i);
}
function suf(s, i) {
    var off = s.length;
    iterateString(s, function(_, idx, sidx) { if (idx == i) { off = sidx; return false; } });
    return s.substring(off);
}
function slenUtf8(s) {
    return s.length;
}
function slen(s) {
    var len = 0;
    iterateString(s, function(){ ++len;});
    return len;
}
function sidx(s, ch) {
    var r = -1;
    iterateString(s, function(c, idx){ if (c == ch) { r = idx; return false; } });
    if (r == -1)
        return null;
    else
        return r;
}
function ssidx(h, n) {
    if (n == "") return 0;
    var ah = strSplit(h);
    var an = strSplit(n);
    var i = 0, y = 0;
    var top = ah.length - an.length + 1;
    if (top < 0) top = 0;
    var found = true;
    
    for(i = 0; i < top; ++i) {
	found = true;
	
	for (y = 0; y < an.length; ++y) {
	    if (ah[i + y] != an[y]) {
		found = false;
		break;
	    }		
	}

	if (found)
	    return i;
    }
    return null;
}

function sspn(s, chs) {
    var s2 = strSplit(s);
    var chs2 = strSplit(chs);
    
    for (var i = 0; i < s2.length; ++i)
        if (chs2.indexOf(s2[i]) != -1)
            return i;

    return s2.length;
}

function schr(s, ch) {
    var r = s.indexOf(ch);
    if (r == -1)
        return null;
    else
        return s.substring(r);
}
function ssub(s, start, len) {
    return strSplit(s).slice(start, start+len).join("");
}
function strlenGe(s, len) {
    return slen(s) >= len;
}

function trimZeroes(s) {
    for (var i = 0; i < s.length; ++i)
        if (s.charAt(i) != '0') {
            if (i > 0)
                return s.substring(i);
            else
                return s;
        }

    if (s.length == 0)
        return s;
    else
        return "0";
}

function pi(s) {
    var st = trimZeroes(s);
    var r = parseInt(st);
    if (r.toString() == st)
        return r;
    else
        er("Can't parse int: " + s);
}

function pfl(s) {
    var r = parseFloat(s);
    if (r.toString() == s)
        return r;
    else
        er("Can't parse float: " + s);
}

function pio(s) {
    var st = trimZeroes(s);
    var r = parseInt(st);
    if (r.toString() == st)
        return r;
    else
        return null;
}

function pflo(s) {
    var r = parseFloat(s);
    if (r.toString() == s)
        return r;
    else
        return null;
}

function parseSource(s1, s2) {
    return eval("s" + s1 + "_" + s2);
}

function uf(s) {
    if (s.length == 0)
        return "_";
    s = s.replace(/\./g, ".2E");
    return (s.charAt(0) == '_' ? "_" : "") + encodeURIComponent(s).replace(/%/g, ".");
}

function uu(s) {
    if (s.length > 0 && s.charAt(0) == '_') {
        s = s.substring(1);
    } else if (s.length >= 3 && (s.charAt(0) == '%' || s.charAt(0) == '.')
               && s.charAt(1) == '5' && (s.charAt(2) == 'f' || s.charAt(2) == 'F'))
        s = s.substring(3);
    s = s.replace(/\+/g, " ");
    s = s.replace(/\./g, "%");
    return decodeURIComponent(s);
}

function atr(s) {
    return s.replace(/\"/g, "&quot;").replace(/&/g, "&amp;")
}

function ub(b) {
    return b ? "1" : "0";
}

function uul(getToken, getData) {
    var tok = getToken();
    if (tok == "Nil") {
        return null;
    } else if (tok == "Cons") {
        var d = getData();
        var l = uul(getToken, getData);
        return {_1:d, _2:l};
    } else
        whine("Can't unmarshal list (" + tok + ")");
}

function strcmp(str1, str2) {
    return ((str1 == str2) ? 0 : ((str1 > str2) ? 1 : -1));
}

function chr(n) {
    return String.fromCodePoint(n);
}

function htmlifySpecialChar(ch) {
    return "&#" + ch.codePointAt(0) + ";";
}


// Remote calls

var client_id = null;
var client_pass = 0;
var url_prefix = "/";
var timeout = 60;
var isPost = false;

function getXHR(uri)
{
    try {
        return new XMLHttpRequest();
    } catch (e) {
        try {
            return new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                return new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e) {
                whine("Your browser doesn't seem to support AJAX.");
            }
        }
    }
}

var sig = null;

var unloading = false, inFlight = null;

function unload() {
    for (; inFlight; inFlight = inFlight.next) {
        inFlight.data.abort();
    }
}

function requestUri(xhr, full_uri, needsSig, isRpc) {
    var extraData = null;
    uri = full_uri;

    if (isRpc && uri.length > 2000) {
        extraData = uri.substring(2000);
        uri = uri.substring(0, 2000);
    }

    xhr.open("POST", uri, !unloading);
    xhr.setRequestHeader("Content-type", "text/plain");

    if (client_id != null) {
        xhr.setRequestHeader("UrWeb-Client", client_id.toString());
        xhr.setRequestHeader("UrWeb-Pass", client_pass.toString());
    }

    if (needsSig) {
        if (sig == null) {
            if (document.readyState == 'complete')
                whine("Missing cookie signature!");

            // The page wasn't loaded yet, which prevented the signature from being set.
            // Wait for it to be ready.
            old_onload = window.onload;
            window.onload = function() {
                if (old_onload) old_onload();
                requestUri(xhr, full_uri, needsSig, isRpc);
            };
            return;
        }

        xhr.setRequestHeader("UrWeb-Sig", sig);
    }

    inFlight = cons(xhr, inFlight);
    xhr.send(extraData);
}

function xhrFinished(xhr) {
    xhr.abort();
    inFlight = remove(xhr, inFlight);
}

function unurlify(parse, s) {
    return parse(s);
}

function redirect(s) {
    if (s.length > 0 && s[s.length-1] == '#')
        s = s.substr(0, s.length-1);
    window.location = s;
}

function makeSome(isN, v) {
    if (isN)
        return {v: v};
    else
        return v;
}

function rc(prefix, uri, parse, k, needsSig, isN) {
    if (!maySuspend)
        er("May not 'rpc' in main thread of 'code' for <active>");

    uri = cat(prefix, uri);
    uri = flattenLocal(uri);
    var xhr = getXHR();

    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            var isok = false;

            try {
                if (xhr.status == 200)
                    isok = true;
            } catch (e) { }

            if (isok) {
                var lines = xhr.responseText.split("\n");
                if (lines.length != 2) {
                    if (isN == null)
                        whine("Bad RPC response lines");
                    else
                        k(null);
                } else {
                    eval(lines[0]);

                    try {
                        var v = parse(lines[1]);
                        try {
                            k(makeSome(isN, v));
                        } catch (v) {
                            doExn(v);
                        }
                    } catch (v) {
                        k(null);
                    }
                }
            } else {
                if (isN == null)
                    conn(xhr.responseText);
                else
                    k(null);
            }

            xhrFinished(xhr);
        }
    };

    requestUri(xhr, uri, needsSig, true);
}

function path_join(s1, s2) {
    if (s1.length > 0 && s1.charAt(s1.length-1) == '/')
        return s1 + s2;
    else
        return s1 + "/" + s2;
}

var channels = [];

function newQueue() {
    return { front : null, back : null };
}
function enqueue(q, v) {
    if (q.front == null) {
        q.front = cons(v, null);
        q.back = q.front;
    } else {
        var node = cons(v, null);
        q.back.next = node;
        q.back = node;
    }
}
function dequeue(q) {
    if (q.front == null)
        return null;
    else {
        var r = q.front.data;
        q.front = q.front.next;
        if (q.front == null)
            q.back = null;
        return r;
    }
}

function newChannel() {
    return { msgs : newQueue(), listeners : newQueue() };
}

function listener() {
    var uri = path_join(url_prefix, ".msgs");
    var xhr = getXHR();
    var tid, orsc, onTimeout, lastTick;

    var connect = function () {
        xhr.onreadystatechange = orsc;
        lastTick = new Date().getTime();
        tid = window.setTimeout(onTimeout, timeout * 500);
        requestUri(xhr, uri, false, false);
    }

    orsc = function() {
        if (xhr.readyState == 4) {
            window.clearTimeout(tid);

            var isok = false;

            try {
                if (xhr.status == 200)
                    isok = true;
            } catch (e) { }

            if (isok) {
                var text = xhr.responseText;
                if (text == "")
                    return;
                var lines = text.split("\n");

                if (lines.length == 1 && lines[0] == "R") {
                    lameDuck = true;

                    if (isPost)
                        history.back();
                    else
                        location.reload();

                    return;
                }

                if (lines.length < 2) {
                    discon();
                    return;
                }

                var messageReader = function(i) {
                    if (i+1 >= lines.length) {
                        xhrFinished(xhr);
                        connect();
                    }
                    else {
                        var chn = lines[i];
                        var msg = lines[i+1];

                        if (chn == "E") {
                            eval(msg);
                            window.setTimeout(function() { messageReader(i+2); }, 0);
                        } else {
                            if (chn < 0)
                                whine("Out-of-bounds channel in message from remote server");

                            var ch;

                            if (chn >= channels.length || channels[chn] == null) {
                                ch = newChannel();
                                channels[chn] = ch;
                            } else
                                ch = channels[chn];

                            var listener = dequeue(ch.listeners);
                            if (listener == null) {
                                enqueue(ch.msgs, msg);
                            } else {
                                try {
                                    listener(msg);
                                } catch (v) {
                                    doExn(v);
                                }
                            }

                            messageReader(i+2);
                        }
                    }
                }

                messageReader(0);
            }
            else {
                try {
                    if (xhr.status != 0)
                        servErr("Error querying remote server for messages: " + xhr.status);
                } catch (e) { }
            }
        }
    };

    onTimeout = function() {
        var thisTick = new Date().getTime();
        xhrFinished(xhr);

        if (thisTick - lastTick > timeout * 1000) {
            if (confirm("The session for this page has expired.  Please choose \"OK\" to reload.")) {
                if (isPost)
                    history.back();
                else
                    location.reload();
            }
        } else {
            connect();
        }
    };

    connect();
}

function rv(chn, parse, k) {
    if (!maySuspend)
        er("May not 'recv' in main thread of 'code' for <active>");

    if (chn == null)
        er("Client-side code tried to recv() from a channel belonging to a different page view.");

    if (chn < 0)
        whine("Out-of-bounds channel receive");

    var ch;

    if (chn >= channels.length || channels[chn] == null) {
        ch = newChannel();
        channels[chn] = ch;
    } else
        ch = channels[chn];

    var msg = dequeue(ch.msgs);
    if (msg == null) {
        enqueue(ch.listeners, function(msg) { k(parse(msg)); });
    } else {
        try {
            k(parse(msg));
        } catch (v) {
            doExn(v);
        }
    }
}

function sl(ms, k) {
    if (!maySuspend)
        er("May not 'sleep' in main thread of 'code' for <active>");

    window.setTimeout(function() { k(null); }, ms);
}

function sp(e) {
    window.setTimeout(function() { execF(e); }, 0);
}


// The Ur interpreter

var urfuncs = [];

function lookup(env, n) {
    while (env != null) {
        if (n == 0)
            return env.data;
        else {
            --n;
            env = env.next;
        }
    }

    whine("Out-of-bounds Ur variable reference");
}

function execP(env, p, v) {
    switch (p.c) {
    case "v":
        return cons(v, env);
    case "c":
        if (v == p.v)
            return env;
        else
            return false;
    case "s":
        if (v == null)
            return false;
        else
            return execP(env, p.p, p.n ? v.v : v);
    case "1":
        if (v.n != p.n)
            return false;
        else
            return execP(env, p.p, v.v);
    case "r":
        for (var fs = p.l; fs != null; fs = fs.next) {
            env = execP(env, fs.data.p, v["_" + fs.data.n]);
            if (env == false)
                return false;
        }
        return env;
    default:
        whine("Unknown Ur pattern kind " + p.c);
    }
}

function exec0(env, e) {
    return exec1(env, null, e);
}

function exec1(env, stack, e) {
    var stack, usedK = false;

    var saveEnv = function() {
        if (stack.next != null && stack.next.data.c != "<")
            stack = cons({c: "<", env: env}, stack.next);
        else
            stack = stack.next;
    };

    while (true) {
        switch (e.c) {
        case "c":
            var v = e.v;
            if (stack == null)
                return v;
            var fr = stack.data;

            switch (fr.c) {
            case "s":
                e = {c: "c", v: {v: v}};
                stack = stack.next;
                break;
            case "1":
                e = {c: "c", v: {n: fr.n, v: v}};
                stack = stack.next;
                break;
            case "f":
                fr.args[fr.pos++] = v;
                if (fr.a == null) {
                    var res;
                    stack = stack.next;

                    if (fr.f.apply)
                        res = fr.f.apply(null, fr.args);
                    else if (fr.args.length == 0)
                        res = fr.f();
                    else if (fr.args.length == 1)
                        res = fr.f(fr.args[0]);
                    else if (fr.args.length == 2)
                        res = fr.f(fr.args[0], fr.args[1]);
                    else if (fr.args.length == 3)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2]);
                    else if (fr.args.length == 4)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2], fr.args[3]);
                    else if (fr.args.length == 5)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2], fr.args[3], fr.args[4]);
                    else
                        whine("Native function has " + fr.args.length + " args, but there is no special case for that count.");

                    e = {c: "c", v: res};
                    if (usedK) return null;
                } else {
                    e = fr.a.data;
                    fr.a = fr.a.next;
                }
                break;
            case "a1":
                e = fr.x;
                stack = cons({c: "a2", f: v}, stack.next);
                break;
            case "a2":
                if (fr.f == null)
                    whine("Ur: applying null function");
                else if (fr.f.body) {
                    saveEnv();
                    env = cons(v, fr.f.env);
                    e = fr.f.body;
                } else {
                    e = {c: "c", v: fr.f(v)};
                    stack = stack.next;
                }
                break;
            case "<":
                env = fr.env;
                stack = stack.next;
                break;
            case "r":
                fr.fs["_" + fr.n] = v;
                if (fr.l == null) {
                    e = {c: "c", v: fr.fs};
                    stack = stack.next;
                } else {
                    fr.n = fr.l.data.n;
                    e = fr.l.data.v;
                    fr.l = fr.l.next;
                }
                break;
            case ".":
                e = {c: "c", v: v["_" + fr.f]};
                stack = stack.next;
                break;
            case ";":
                e = fr.e2;
                stack = stack.next;
                break;
            case "=":
                saveEnv();
                env = cons(v, env);
                e = fr.e2;
                break;
            case "m":
                var ps;
                for (ps = fr.p; ps != null; ps = ps.next) {
                    var r = execP(env, ps.data.p, v);
                    if (r != false) {
                        saveEnv();
                        env = r;
                        e = ps.data.b;
                        break;
                    }
                }
                if (ps == null)
                    whine("Match failure in Ur interpretation");
                break;
            default:
                whine("Unknown Ur continuation kind " + fr.c);
            }

            break;
        case "v":
            e = {c: "c", v: lookup(env, e.n)};
            break;
        case "n":
            var idx = e.n;
            e = urfuncs[idx];
            if (e.c == "t")
                e = urfuncs[idx] = eval("(" + e.f + ")");
            break;
        case "s":
            stack = cons({c: "s"}, stack);
            e = e.v;
            break;
        case "1":
            stack = cons({c: "1", n: e.n}, stack);
            e = e.v;
            break;
        case "f":
            if (e.a == null)
                e = {c: "c", v: e.f()};
            else {
                var args = [];
                stack = cons({c: "f", f: e.f, args: args, pos: 0, a: e.a.next}, stack);
                if (!e.a.data.c) alert("[2] fr.f = " + e.f + "; 0 = " + e.a.data);
                e = e.a.data;
            }
            break;
        case "l":
            e = {c: "c", v: {env: env, body: e.b}};
            break;
        case "a":
            stack = cons({c: "a1", x: e.x}, stack);
            e = e.f;
            break;
        case "r":
            if (e.l == null)
                whine("Empty Ur record in interpretation");
            var fs = {};
            stack = cons({c: "r", n: e.l.data.n, fs: fs, l: e.l.next}, stack);
            e = e.l.data.v;
            break;
        case ".":
            stack = cons({c: ".", f: e.f}, stack);
            e = e.r;
            break;
        case ";":
            stack = cons({c: ";", e2: e.e2}, stack);
            e = e.e1;
            break;
        case "=":
            stack = cons({c: "=", e2: e.e2}, stack);
            e = e.e1;
            break;
        case "m":
            stack = cons({c: "m", p: e.p}, stack);
            e = e.e;
            break;
        case "e":
            e = {c: "c", v: cs({c: "wc", env: env, body: e.e})};
            break;
        case "wc":
            env = e.env;
            e = e.body;
            break;
        case "K":
            { var savedStack = stack.next, savedEnv = env;
                e = {c: "c", v: function(v) { return exec1(savedEnv, savedStack, {c: "c", v: v}); } };}
            usedK = true;
            break;
        default:
            whine("Unknown Ur expression kind " + e.c);
        }
    }
}

function execD(e) {
    return exec0(null, e);
}

function exec(e) {
    var r = exec0(null, e);

    if (r != null && r.body != null)
        return function(v) { return exec0(cons(v, r.env), r.body); };
    else
        return r;
}

function execF(f, x) {
    return exec0(cons(x, f.env), f.body);
}


// Wrappers

function confrm(s) {
    return confirm(s) ? true : false;
}

function currentUrl() {
    return window.location.toString();
}


// URL blessing

var urlRules = null;

function checkUrl(s) {
    for (var r = urlRules; r; r = r.next) {
        var ru = r.data;
        if (ru.prefix ? s.indexOf(ru.pattern) == 0 : s == ru.pattern)
            return ru.allow ? s : null;
    }

    return null;
}

function bless(s) {
    u = checkUrl(s);
    if (u == null)
        er("Disallowed URL: " + s);
    return u;
}


// Attribute name blessing
var maxCh = chr(127);

function blessData(s) {
    var chars = strSplit(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != '-' && c != '_'))
            er("Disallowed character in data-* attribute name");
    }

    return s;
}


// CSS validation

function atom(s) {
    var chars = strSplit(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != '+' && c != '-' && c != '.' && c != '%' && c != '#'))
            er("Disallowed character in CSS atom");
    }

    return s;
}

function css_url(s) {
    var chars = strSplit(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != ':' && c != '/' && c != '.' && c != '_' && c != '+'
			  && c != '-' && c != '%' && c != '?' && c != '&' && c != '=' && c != '#'))
            er("Disallowed character in CSS URL");
    }

    return s;
}

function property(s) {
    var chars = strSplit(s);

    if (chars.length <= 0)
        er("Empty CSS property");

    // See https://www.w3.org/TR/CSS21/grammar.html#scanner, rule `ident`
    function nmstart(c) {
      return c <= maxCh && (isAlpha(c) || c == '_')
    }
    function nmchar(c) {
      return c <= maxCh && (isAlpha(c) || isDigit(c) || c == '_' || c == '-')
    }

    if (!(nmstart(chars[0]) || chars.length > 1 && chars[0] == '-' && nmstart(chars[1])))
        er("Bad initial character in CSS property");

    for (var i = 0; i < chars.length; ++i) {
        if (!(nmchar(chars[i])))
	    er("Disallowed character in CSS property");
    }

    return s;
}


// ID generation

var nextId = 0;

function fresh() {
    return "uw" + (--nextId);
}

function giveFocus(id) {
    var node = document.getElementById(id);

    if (node)
        node.focus();
    else
        er("Tried to give focus to ID not used in document: " + id);
}

function anchorUrl(id) {
    return "#" + id;
}


// App-specific code
