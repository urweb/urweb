// Detect browser quirks that we should be aware of.

function needsDynPrefix() {
    var span = document.createElement("span");
    span.innerHTML = "<script>alert('test');</script>";
    var scripts = span.getElementsByTagName("script");
    return scripts.length == 0;
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

function isLower(c) {
    var cp = ord(c);

    if (cp >= 97 && cp <= 122) return true;
    if (cp == 170) return true;
    if (cp == 181) return true;
    if (cp == 186) return true;
    if (cp >= 223 && cp <= 246) return true;
    if (cp >= 248 && cp <= 255) return true;
    if (cp == 257) return true;
    if (cp == 259) return true;
    if (cp == 261) return true;
    if (cp == 263) return true;
    if (cp == 265) return true;
    if (cp == 267) return true;
    if (cp == 269) return true;
    if (cp == 271) return true;
    if (cp == 273) return true;
    if (cp == 275) return true;
    if (cp == 277) return true;
    if (cp == 279) return true;
    if (cp == 281) return true;
    if (cp == 283) return true;
    if (cp == 285) return true;
    if (cp == 287) return true;
    if (cp == 289) return true;
    if (cp == 291) return true;
    if (cp == 293) return true;
    if (cp == 295) return true;
    if (cp == 297) return true;
    if (cp == 299) return true;
    if (cp == 301) return true;
    if (cp == 303) return true;
    if (cp == 305) return true;
    if (cp == 307) return true;
    if (cp == 309) return true;
    if (cp >= 311 && cp <= 312) return true;
    if (cp == 314) return true;
    if (cp == 316) return true;
    if (cp == 318) return true;
    if (cp == 320) return true;
    if (cp == 322) return true;
    if (cp == 324) return true;
    if (cp == 326) return true;
    if (cp >= 328 && cp <= 329) return true;
    if (cp == 331) return true;
    if (cp == 333) return true;
    if (cp == 335) return true;
    if (cp == 337) return true;
    if (cp == 339) return true;
    if (cp == 341) return true;
    if (cp == 343) return true;
    if (cp == 345) return true;
    if (cp == 347) return true;
    if (cp == 349) return true;
    if (cp == 351) return true;
    if (cp == 353) return true;
    if (cp == 355) return true;
    if (cp == 357) return true;
    if (cp == 359) return true;
    if (cp == 361) return true;
    if (cp == 363) return true;
    if (cp == 365) return true;
    if (cp == 367) return true;
    if (cp == 369) return true;
    if (cp == 371) return true;
    if (cp == 373) return true;
    if (cp == 375) return true;
    if (cp == 378) return true;
    if (cp == 380) return true;
    if (cp >= 382 && cp <= 384) return true;
    if (cp == 387) return true;
    if (cp == 389) return true;
    if (cp == 392) return true;
    if (cp >= 396 && cp <= 397) return true;
    if (cp == 402) return true;
    if (cp == 405) return true;
    if (cp >= 409 && cp <= 411) return true;
    if (cp == 414) return true;
    if (cp == 417) return true;
    if (cp == 419) return true;
    if (cp == 421) return true;
    if (cp == 424) return true;
    if (cp >= 426 && cp <= 427) return true;
    if (cp == 429) return true;
    if (cp == 432) return true;
    if (cp == 436) return true;
    if (cp == 438) return true;
    if (cp >= 441 && cp <= 442) return true;
    if (cp >= 445 && cp <= 447) return true;
    if (cp == 454) return true;
    if (cp == 457) return true;
    if (cp == 460) return true;
    if (cp == 462) return true;
    if (cp == 464) return true;
    if (cp == 466) return true;
    if (cp == 468) return true;
    if (cp == 470) return true;
    if (cp == 472) return true;
    if (cp == 474) return true;
    if (cp >= 476 && cp <= 477) return true;
    if (cp == 479) return true;
    if (cp == 481) return true;
    if (cp == 483) return true;
    if (cp == 485) return true;
    if (cp == 487) return true;
    if (cp == 489) return true;
    if (cp == 491) return true;
    if (cp == 493) return true;
    if (cp >= 495 && cp <= 496) return true;
    if (cp == 499) return true;
    if (cp == 501) return true;
    if (cp == 505) return true;
    if (cp == 507) return true;
    if (cp == 509) return true;
    if (cp == 511) return true;
    if (cp == 513) return true;
    if (cp == 515) return true;
    if (cp == 517) return true;
    if (cp == 519) return true;
    if (cp == 521) return true;
    if (cp == 523) return true;
    if (cp == 525) return true;
    if (cp == 527) return true;
    if (cp == 529) return true;
    if (cp == 531) return true;
    if (cp == 533) return true;
    if (cp == 535) return true;
    if (cp == 537) return true;
    if (cp == 539) return true;
    if (cp == 541) return true;
    if (cp == 543) return true;
    if (cp == 545) return true;
    if (cp == 547) return true;
    if (cp == 549) return true;
    if (cp == 551) return true;
    if (cp == 553) return true;
    if (cp == 555) return true;
    if (cp == 557) return true;
    if (cp == 559) return true;
    if (cp == 561) return true;
    if (cp >= 563 && cp <= 569) return true;
    if (cp == 572) return true;
    if (cp >= 575 && cp <= 576) return true;
    if (cp == 578) return true;
    if (cp == 583) return true;
    if (cp == 585) return true;
    if (cp == 587) return true;
    if (cp == 589) return true;
    if (cp >= 591 && cp <= 659) return true;
    if (cp >= 661 && cp <= 696) return true;
    if (cp >= 704 && cp <= 705) return true;
    if (cp >= 736 && cp <= 740) return true;
    if (cp == 837) return true;
    if (cp == 881) return true;
    if (cp == 883) return true;
    if (cp == 887) return true;
    if (cp >= 890 && cp <= 893) return true;
    if (cp == 912) return true;
    if (cp >= 940 && cp <= 974) return true;
    if (cp >= 976 && cp <= 977) return true;
    if (cp >= 981 && cp <= 983) return true;
    if (cp == 985) return true;
    if (cp == 987) return true;
    if (cp == 989) return true;
    if (cp == 991) return true;
    if (cp == 993) return true;
    if (cp == 995) return true;
    if (cp == 997) return true;
    if (cp == 999) return true;
    if (cp == 1001) return true;
    if (cp == 1003) return true;
    if (cp == 1005) return true;
    if (cp >= 1007 && cp <= 1011) return true;
    if (cp == 1013) return true;
    if (cp == 1016) return true;
    if (cp >= 1019 && cp <= 1020) return true;
    if (cp >= 1072 && cp <= 1119) return true;
    if (cp == 1121) return true;
    if (cp == 1123) return true;
    if (cp == 1125) return true;
    if (cp == 1127) return true;
    if (cp == 1129) return true;
    if (cp == 1131) return true;
    if (cp == 1133) return true;
    if (cp == 1135) return true;
    if (cp == 1137) return true;
    if (cp == 1139) return true;
    if (cp == 1141) return true;
    if (cp == 1143) return true;
    if (cp == 1145) return true;
    if (cp == 1147) return true;
    if (cp == 1149) return true;
    if (cp == 1151) return true;
    if (cp == 1153) return true;
    if (cp == 1163) return true;
    if (cp == 1165) return true;
    if (cp == 1167) return true;
    if (cp == 1169) return true;
    if (cp == 1171) return true;
    if (cp == 1173) return true;
    if (cp == 1175) return true;
    if (cp == 1177) return true;
    if (cp == 1179) return true;
    if (cp == 1181) return true;
    if (cp == 1183) return true;
    if (cp == 1185) return true;
    if (cp == 1187) return true;
    if (cp == 1189) return true;
    if (cp == 1191) return true;
    if (cp == 1193) return true;
    if (cp == 1195) return true;
    if (cp == 1197) return true;
    if (cp == 1199) return true;
    if (cp == 1201) return true;
    if (cp == 1203) return true;
    if (cp == 1205) return true;
    if (cp == 1207) return true;
    if (cp == 1209) return true;
    if (cp == 1211) return true;
    if (cp == 1213) return true;
    if (cp == 1215) return true;
    if (cp == 1218) return true;
    if (cp == 1220) return true;
    if (cp == 1222) return true;
    if (cp == 1224) return true;
    if (cp == 1226) return true;
    if (cp == 1228) return true;
    if (cp >= 1230 && cp <= 1231) return true;
    if (cp == 1233) return true;
    if (cp == 1235) return true;
    if (cp == 1237) return true;
    if (cp == 1239) return true;
    if (cp == 1241) return true;
    if (cp == 1243) return true;
    if (cp == 1245) return true;
    if (cp == 1247) return true;
    if (cp == 1249) return true;
    if (cp == 1251) return true;
    if (cp == 1253) return true;
    if (cp == 1255) return true;
    if (cp == 1257) return true;
    if (cp == 1259) return true;
    if (cp == 1261) return true;
    if (cp == 1263) return true;
    if (cp == 1265) return true;
    if (cp == 1267) return true;
    if (cp == 1269) return true;
    if (cp == 1271) return true;
    if (cp == 1273) return true;
    if (cp == 1275) return true;
    if (cp == 1277) return true;
    if (cp == 1279) return true;
    if (cp == 1281) return true;
    if (cp == 1283) return true;
    if (cp == 1285) return true;
    if (cp == 1287) return true;
    if (cp == 1289) return true;
    if (cp == 1291) return true;
    if (cp == 1293) return true;
    if (cp == 1295) return true;
    if (cp == 1297) return true;
    if (cp == 1299) return true;
    if (cp == 1301) return true;
    if (cp == 1303) return true;
    if (cp == 1305) return true;
    if (cp == 1307) return true;
    if (cp == 1309) return true;
    if (cp == 1311) return true;
    if (cp == 1313) return true;
    if (cp == 1315) return true;
    if (cp == 1317) return true;
    if (cp == 1319) return true;
    if (cp == 1321) return true;
    if (cp == 1323) return true;
    if (cp == 1325) return true;
    if (cp == 1327) return true;
    if (cp >= 1377 && cp <= 1415) return true;
    if (cp >= 5112 && cp <= 5117) return true;
    if (cp >= 7296 && cp <= 7304) return true;
    if (cp >= 7424 && cp <= 7615) return true;
    if (cp == 7681) return true;
    if (cp == 7683) return true;
    if (cp == 7685) return true;
    if (cp == 7687) return true;
    if (cp == 7689) return true;
    if (cp == 7691) return true;
    if (cp == 7693) return true;
    if (cp == 7695) return true;
    if (cp == 7697) return true;
    if (cp == 7699) return true;
    if (cp == 7701) return true;
    if (cp == 7703) return true;
    if (cp == 7705) return true;
    if (cp == 7707) return true;
    if (cp == 7709) return true;
    if (cp == 7711) return true;
    if (cp == 7713) return true;
    if (cp == 7715) return true;
    if (cp == 7717) return true;
    if (cp == 7719) return true;
    if (cp == 7721) return true;
    if (cp == 7723) return true;
    if (cp == 7725) return true;
    if (cp == 7727) return true;
    if (cp == 7729) return true;
    if (cp == 7731) return true;
    if (cp == 7733) return true;
    if (cp == 7735) return true;
    if (cp == 7737) return true;
    if (cp == 7739) return true;
    if (cp == 7741) return true;
    if (cp == 7743) return true;
    if (cp == 7745) return true;
    if (cp == 7747) return true;
    if (cp == 7749) return true;
    if (cp == 7751) return true;
    if (cp == 7753) return true;
    if (cp == 7755) return true;
    if (cp == 7757) return true;
    if (cp == 7759) return true;
    if (cp == 7761) return true;
    if (cp == 7763) return true;
    if (cp == 7765) return true;
    if (cp == 7767) return true;
    if (cp == 7769) return true;
    if (cp == 7771) return true;
    if (cp == 7773) return true;
    if (cp == 7775) return true;
    if (cp == 7777) return true;
    if (cp == 7779) return true;
    if (cp == 7781) return true;
    if (cp == 7783) return true;
    if (cp == 7785) return true;
    if (cp == 7787) return true;
    if (cp == 7789) return true;
    if (cp == 7791) return true;
    if (cp == 7793) return true;
    if (cp == 7795) return true;
    if (cp == 7797) return true;
    if (cp == 7799) return true;
    if (cp == 7801) return true;
    if (cp == 7803) return true;
    if (cp == 7805) return true;
    if (cp == 7807) return true;
    if (cp == 7809) return true;
    if (cp == 7811) return true;
    if (cp == 7813) return true;
    if (cp == 7815) return true;
    if (cp == 7817) return true;
    if (cp == 7819) return true;
    if (cp == 7821) return true;
    if (cp == 7823) return true;
    if (cp == 7825) return true;
    if (cp == 7827) return true;
    if (cp >= 7829 && cp <= 7837) return true;
    if (cp == 7839) return true;
    if (cp == 7841) return true;
    if (cp == 7843) return true;
    if (cp == 7845) return true;
    if (cp == 7847) return true;
    if (cp == 7849) return true;
    if (cp == 7851) return true;
    if (cp == 7853) return true;
    if (cp == 7855) return true;
    if (cp == 7857) return true;
    if (cp == 7859) return true;
    if (cp == 7861) return true;
    if (cp == 7863) return true;
    if (cp == 7865) return true;
    if (cp == 7867) return true;
    if (cp == 7869) return true;
    if (cp == 7871) return true;
    if (cp == 7873) return true;
    if (cp == 7875) return true;
    if (cp == 7877) return true;
    if (cp == 7879) return true;
    if (cp == 7881) return true;
    if (cp == 7883) return true;
    if (cp == 7885) return true;
    if (cp == 7887) return true;
    if (cp == 7889) return true;
    if (cp == 7891) return true;
    if (cp == 7893) return true;
    if (cp == 7895) return true;
    if (cp == 7897) return true;
    if (cp == 7899) return true;
    if (cp == 7901) return true;
    if (cp == 7903) return true;
    if (cp == 7905) return true;
    if (cp == 7907) return true;
    if (cp == 7909) return true;
    if (cp == 7911) return true;
    if (cp == 7913) return true;
    if (cp == 7915) return true;
    if (cp == 7917) return true;
    if (cp == 7919) return true;
    if (cp == 7921) return true;
    if (cp == 7923) return true;
    if (cp == 7925) return true;
    if (cp == 7927) return true;
    if (cp == 7929) return true;
    if (cp == 7931) return true;
    if (cp == 7933) return true;
    if (cp >= 7935 && cp <= 7943) return true;
    if (cp >= 7952 && cp <= 7957) return true;
    if (cp >= 7968 && cp <= 7975) return true;
    if (cp >= 7984 && cp <= 7991) return true;
    if (cp >= 8000 && cp <= 8005) return true;
    if (cp >= 8016 && cp <= 8023) return true;
    if (cp >= 8032 && cp <= 8039) return true;
    if (cp >= 8048 && cp <= 8061) return true;
    if (cp >= 8064 && cp <= 8071) return true;
    if (cp >= 8080 && cp <= 8087) return true;
    if (cp >= 8096 && cp <= 8103) return true;
    if (cp >= 8112 && cp <= 8116) return true;
    if (cp >= 8118 && cp <= 8119) return true;
    if (cp == 8126) return true;
    if (cp >= 8130 && cp <= 8132) return true;
    if (cp >= 8134 && cp <= 8135) return true;
    if (cp >= 8144 && cp <= 8147) return true;
    if (cp >= 8150 && cp <= 8151) return true;
    if (cp >= 8160 && cp <= 8167) return true;
    if (cp >= 8178 && cp <= 8180) return true;
    if (cp >= 8182 && cp <= 8183) return true;
    if (cp == 8305) return true;
    if (cp == 8319) return true;
    if (cp >= 8336 && cp <= 8348) return true;
    if (cp == 8458) return true;
    if (cp >= 8462 && cp <= 8463) return true;
    if (cp == 8467) return true;
    if (cp == 8495) return true;
    if (cp == 8500) return true;
    if (cp == 8505) return true;
    if (cp >= 8508 && cp <= 8509) return true;
    if (cp >= 8518 && cp <= 8521) return true;
    if (cp == 8526) return true;
    if (cp >= 8560 && cp <= 8575) return true;
    if (cp == 8580) return true;
    if (cp >= 9424 && cp <= 9449) return true;
    if (cp >= 11312 && cp <= 11358) return true;
    if (cp == 11361) return true;
    if (cp >= 11365 && cp <= 11366) return true;
    if (cp == 11368) return true;
    if (cp == 11370) return true;
    if (cp == 11372) return true;
    if (cp == 11377) return true;
    if (cp >= 11379 && cp <= 11380) return true;
    if (cp >= 11382 && cp <= 11389) return true;
    if (cp == 11393) return true;
    if (cp == 11395) return true;
    if (cp == 11397) return true;
    if (cp == 11399) return true;
    if (cp == 11401) return true;
    if (cp == 11403) return true;
    if (cp == 11405) return true;
    if (cp == 11407) return true;
    if (cp == 11409) return true;
    if (cp == 11411) return true;
    if (cp == 11413) return true;
    if (cp == 11415) return true;
    if (cp == 11417) return true;
    if (cp == 11419) return true;
    if (cp == 11421) return true;
    if (cp == 11423) return true;
    if (cp == 11425) return true;
    if (cp == 11427) return true;
    if (cp == 11429) return true;
    if (cp == 11431) return true;
    if (cp == 11433) return true;
    if (cp == 11435) return true;
    if (cp == 11437) return true;
    if (cp == 11439) return true;
    if (cp == 11441) return true;
    if (cp == 11443) return true;
    if (cp == 11445) return true;
    if (cp == 11447) return true;
    if (cp == 11449) return true;
    if (cp == 11451) return true;
    if (cp == 11453) return true;
    if (cp == 11455) return true;
    if (cp == 11457) return true;
    if (cp == 11459) return true;
    if (cp == 11461) return true;
    if (cp == 11463) return true;
    if (cp == 11465) return true;
    if (cp == 11467) return true;
    if (cp == 11469) return true;
    if (cp == 11471) return true;
    if (cp == 11473) return true;
    if (cp == 11475) return true;
    if (cp == 11477) return true;
    if (cp == 11479) return true;
    if (cp == 11481) return true;
    if (cp == 11483) return true;
    if (cp == 11485) return true;
    if (cp == 11487) return true;
    if (cp == 11489) return true;
    if (cp >= 11491 && cp <= 11492) return true;
    if (cp == 11500) return true;
    if (cp == 11502) return true;
    if (cp == 11507) return true;
    if (cp >= 11520 && cp <= 11557) return true;
    if (cp == 11559) return true;
    if (cp == 11565) return true;
    if (cp == 42561) return true;
    if (cp == 42563) return true;
    if (cp == 42565) return true;
    if (cp == 42567) return true;
    if (cp == 42569) return true;
    if (cp == 42571) return true;
    if (cp == 42573) return true;
    if (cp == 42575) return true;
    if (cp == 42577) return true;
    if (cp == 42579) return true;
    if (cp == 42581) return true;
    if (cp == 42583) return true;
    if (cp == 42585) return true;
    if (cp == 42587) return true;
    if (cp == 42589) return true;
    if (cp == 42591) return true;
    if (cp == 42593) return true;
    if (cp == 42595) return true;
    if (cp == 42597) return true;
    if (cp == 42599) return true;
    if (cp == 42601) return true;
    if (cp == 42603) return true;
    if (cp == 42605) return true;
    if (cp == 42625) return true;
    if (cp == 42627) return true;
    if (cp == 42629) return true;
    if (cp == 42631) return true;
    if (cp == 42633) return true;
    if (cp == 42635) return true;
    if (cp == 42637) return true;
    if (cp == 42639) return true;
    if (cp == 42641) return true;
    if (cp == 42643) return true;
    if (cp == 42645) return true;
    if (cp == 42647) return true;
    if (cp == 42649) return true;
    if (cp >= 42651 && cp <= 42653) return true;
    if (cp == 42787) return true;
    if (cp == 42789) return true;
    if (cp == 42791) return true;
    if (cp == 42793) return true;
    if (cp == 42795) return true;
    if (cp == 42797) return true;
    if (cp >= 42799 && cp <= 42801) return true;
    if (cp == 42803) return true;
    if (cp == 42805) return true;
    if (cp == 42807) return true;
    if (cp == 42809) return true;
    if (cp == 42811) return true;
    if (cp == 42813) return true;
    if (cp == 42815) return true;
    if (cp == 42817) return true;
    if (cp == 42819) return true;
    if (cp == 42821) return true;
    if (cp == 42823) return true;
    if (cp == 42825) return true;
    if (cp == 42827) return true;
    if (cp == 42829) return true;
    if (cp == 42831) return true;
    if (cp == 42833) return true;
    if (cp == 42835) return true;
    if (cp == 42837) return true;
    if (cp == 42839) return true;
    if (cp == 42841) return true;
    if (cp == 42843) return true;
    if (cp == 42845) return true;
    if (cp == 42847) return true;
    if (cp == 42849) return true;
    if (cp == 42851) return true;
    if (cp == 42853) return true;
    if (cp == 42855) return true;
    if (cp == 42857) return true;
    if (cp == 42859) return true;
    if (cp == 42861) return true;
    if (cp >= 42863 && cp <= 42872) return true;
    if (cp == 42874) return true;
    if (cp == 42876) return true;
    if (cp == 42879) return true;
    if (cp == 42881) return true;
    if (cp == 42883) return true;
    if (cp == 42885) return true;
    if (cp == 42887) return true;
    if (cp == 42892) return true;
    if (cp == 42894) return true;
    if (cp == 42897) return true;
    if (cp >= 42899 && cp <= 42901) return true;
    if (cp == 42903) return true;
    if (cp == 42905) return true;
    if (cp == 42907) return true;
    if (cp == 42909) return true;
    if (cp == 42911) return true;
    if (cp == 42913) return true;
    if (cp == 42915) return true;
    if (cp == 42917) return true;
    if (cp == 42919) return true;
    if (cp == 42921) return true;
    if (cp == 42933) return true;
    if (cp == 42935) return true;
    if (cp >= 43000 && cp <= 43002) return true;
    if (cp >= 43824 && cp <= 43866) return true;
    if (cp >= 43868 && cp <= 43877) return true;
    if (cp >= 43888 && cp <= 43967) return true;
    if (cp >= 64256 && cp <= 64262) return true;
    if (cp >= 64275 && cp <= 64279) return true;
    if (cp >= 65345 && cp <= 65370) return true;
    if (cp >= 66600 && cp <= 66639) return true;
    if (cp >= 66776 && cp <= 66811) return true;
    if (cp >= 68800 && cp <= 68850) return true;
    if (cp >= 71872 && cp <= 71903) return true;
    if (cp >= 119834 && cp <= 119859) return true;
    if (cp >= 119886 && cp <= 119892) return true;
    if (cp >= 119894 && cp <= 119911) return true;
    if (cp >= 119938 && cp <= 119963) return true;
    if (cp >= 119990 && cp <= 119993) return true;
    if (cp == 119995) return true;
    if (cp >= 119997 && cp <= 120003) return true;
    if (cp >= 120005 && cp <= 120015) return true;
    if (cp >= 120042 && cp <= 120067) return true;
    if (cp >= 120094 && cp <= 120119) return true;
    if (cp >= 120146 && cp <= 120171) return true;
    if (cp >= 120198 && cp <= 120223) return true;
    if (cp >= 120250 && cp <= 120275) return true;
    if (cp >= 120302 && cp <= 120327) return true;
    if (cp >= 120354 && cp <= 120379) return true;
    if (cp >= 120406 && cp <= 120431) return true;
    if (cp >= 120458 && cp <= 120485) return true;
    if (cp >= 120514 && cp <= 120538) return true;
    if (cp >= 120540 && cp <= 120545) return true;
    if (cp >= 120572 && cp <= 120596) return true;
    if (cp >= 120598 && cp <= 120603) return true;
    if (cp >= 120630 && cp <= 120654) return true;
    if (cp >= 120656 && cp <= 120661) return true;
    if (cp >= 120688 && cp <= 120712) return true;
    if (cp >= 120714 && cp <= 120719) return true;
    if (cp >= 120746 && cp <= 120770) return true;
    if (cp >= 120772 && cp <= 120777) return true;
    if (cp == 120779) return true;
    if (cp >= 125218 && cp <= 125251) return true;

    return false;
}

function isUpper(c) {
    var cp = ord(c);

    if (cp >= 65 && cp <= 90) return true;
    if (cp >= 192 && cp <= 214) return true;
    if (cp >= 216 && cp <= 222) return true;
    if (cp == 256) return true;
    if (cp == 258) return true;
    if (cp == 260) return true;
    if (cp == 262) return true;
    if (cp == 264) return true;
    if (cp == 266) return true;
    if (cp == 268) return true;
    if (cp == 270) return true;
    if (cp == 272) return true;
    if (cp == 274) return true;
    if (cp == 276) return true;
    if (cp == 278) return true;
    if (cp == 280) return true;
    if (cp == 282) return true;
    if (cp == 284) return true;
    if (cp == 286) return true;
    if (cp == 288) return true;
    if (cp == 290) return true;
    if (cp == 292) return true;
    if (cp == 294) return true;
    if (cp == 296) return true;
    if (cp == 298) return true;
    if (cp == 300) return true;
    if (cp == 302) return true;
    if (cp == 304) return true;
    if (cp == 306) return true;
    if (cp == 308) return true;
    if (cp == 310) return true;
    if (cp == 313) return true;
    if (cp == 315) return true;
    if (cp == 317) return true;
    if (cp == 319) return true;
    if (cp == 321) return true;
    if (cp == 323) return true;
    if (cp == 325) return true;
    if (cp == 327) return true;
    if (cp == 330) return true;
    if (cp == 332) return true;
    if (cp == 334) return true;
    if (cp == 336) return true;
    if (cp == 338) return true;
    if (cp == 340) return true;
    if (cp == 342) return true;
    if (cp == 344) return true;
    if (cp == 346) return true;
    if (cp == 348) return true;
    if (cp == 350) return true;
    if (cp == 352) return true;
    if (cp == 354) return true;
    if (cp == 356) return true;
    if (cp == 358) return true;
    if (cp == 360) return true;
    if (cp == 362) return true;
    if (cp == 364) return true;
    if (cp == 366) return true;
    if (cp == 368) return true;
    if (cp == 370) return true;
    if (cp == 372) return true;
    if (cp == 374) return true;
    if (cp >= 376 && cp <= 377) return true;
    if (cp == 379) return true;
    if (cp == 381) return true;
    if (cp >= 385 && cp <= 386) return true;
    if (cp == 388) return true;
    if (cp >= 390 && cp <= 391) return true;
    if (cp >= 393 && cp <= 395) return true;
    if (cp >= 398 && cp <= 401) return true;
    if (cp >= 403 && cp <= 404) return true;
    if (cp >= 406 && cp <= 408) return true;
    if (cp >= 412 && cp <= 413) return true;
    if (cp >= 415 && cp <= 416) return true;
    if (cp == 418) return true;
    if (cp == 420) return true;
    if (cp >= 422 && cp <= 423) return true;
    if (cp == 425) return true;
    if (cp == 428) return true;
    if (cp >= 430 && cp <= 431) return true;
    if (cp >= 433 && cp <= 435) return true;
    if (cp == 437) return true;
    if (cp >= 439 && cp <= 440) return true;
    if (cp == 444) return true;
    if (cp == 452) return true;
    if (cp == 455) return true;
    if (cp == 458) return true;
    if (cp == 461) return true;
    if (cp == 463) return true;
    if (cp == 465) return true;
    if (cp == 467) return true;
    if (cp == 469) return true;
    if (cp == 471) return true;
    if (cp == 473) return true;
    if (cp == 475) return true;
    if (cp == 478) return true;
    if (cp == 480) return true;
    if (cp == 482) return true;
    if (cp == 484) return true;
    if (cp == 486) return true;
    if (cp == 488) return true;
    if (cp == 490) return true;
    if (cp == 492) return true;
    if (cp == 494) return true;
    if (cp == 497) return true;
    if (cp == 500) return true;
    if (cp >= 502 && cp <= 504) return true;
    if (cp == 506) return true;
    if (cp == 508) return true;
    if (cp == 510) return true;
    if (cp == 512) return true;
    if (cp == 514) return true;
    if (cp == 516) return true;
    if (cp == 518) return true;
    if (cp == 520) return true;
    if (cp == 522) return true;
    if (cp == 524) return true;
    if (cp == 526) return true;
    if (cp == 528) return true;
    if (cp == 530) return true;
    if (cp == 532) return true;
    if (cp == 534) return true;
    if (cp == 536) return true;
    if (cp == 538) return true;
    if (cp == 540) return true;
    if (cp == 542) return true;
    if (cp == 544) return true;
    if (cp == 546) return true;
    if (cp == 548) return true;
    if (cp == 550) return true;
    if (cp == 552) return true;
    if (cp == 554) return true;
    if (cp == 556) return true;
    if (cp == 558) return true;
    if (cp == 560) return true;
    if (cp == 562) return true;
    if (cp >= 570 && cp <= 571) return true;
    if (cp >= 573 && cp <= 574) return true;
    if (cp == 577) return true;
    if (cp >= 579 && cp <= 582) return true;
    if (cp == 584) return true;
    if (cp == 586) return true;
    if (cp == 588) return true;
    if (cp == 590) return true;
    if (cp == 880) return true;
    if (cp == 882) return true;
    if (cp == 886) return true;
    if (cp == 895) return true;
    if (cp == 902) return true;
    if (cp >= 904 && cp <= 906) return true;
    if (cp == 908) return true;
    if (cp >= 910 && cp <= 911) return true;
    if (cp >= 913 && cp <= 929) return true;
    if (cp >= 931 && cp <= 939) return true;
    if (cp == 975) return true;
    if (cp >= 978 && cp <= 980) return true;
    if (cp == 984) return true;
    if (cp == 986) return true;
    if (cp == 988) return true;
    if (cp == 990) return true;
    if (cp == 992) return true;
    if (cp == 994) return true;
    if (cp == 996) return true;
    if (cp == 998) return true;
    if (cp == 1000) return true;
    if (cp == 1002) return true;
    if (cp == 1004) return true;
    if (cp == 1006) return true;
    if (cp == 1012) return true;
    if (cp == 1015) return true;
    if (cp >= 1017 && cp <= 1018) return true;
    if (cp >= 1021 && cp <= 1071) return true;
    if (cp == 1120) return true;
    if (cp == 1122) return true;
    if (cp == 1124) return true;
    if (cp == 1126) return true;
    if (cp == 1128) return true;
    if (cp == 1130) return true;
    if (cp == 1132) return true;
    if (cp == 1134) return true;
    if (cp == 1136) return true;
    if (cp == 1138) return true;
    if (cp == 1140) return true;
    if (cp == 1142) return true;
    if (cp == 1144) return true;
    if (cp == 1146) return true;
    if (cp == 1148) return true;
    if (cp == 1150) return true;
    if (cp == 1152) return true;
    if (cp == 1162) return true;
    if (cp == 1164) return true;
    if (cp == 1166) return true;
    if (cp == 1168) return true;
    if (cp == 1170) return true;
    if (cp == 1172) return true;
    if (cp == 1174) return true;
    if (cp == 1176) return true;
    if (cp == 1178) return true;
    if (cp == 1180) return true;
    if (cp == 1182) return true;
    if (cp == 1184) return true;
    if (cp == 1186) return true;
    if (cp == 1188) return true;
    if (cp == 1190) return true;
    if (cp == 1192) return true;
    if (cp == 1194) return true;
    if (cp == 1196) return true;
    if (cp == 1198) return true;
    if (cp == 1200) return true;
    if (cp == 1202) return true;
    if (cp == 1204) return true;
    if (cp == 1206) return true;
    if (cp == 1208) return true;
    if (cp == 1210) return true;
    if (cp == 1212) return true;
    if (cp == 1214) return true;
    if (cp >= 1216 && cp <= 1217) return true;
    if (cp == 1219) return true;
    if (cp == 1221) return true;
    if (cp == 1223) return true;
    if (cp == 1225) return true;
    if (cp == 1227) return true;
    if (cp == 1229) return true;
    if (cp == 1232) return true;
    if (cp == 1234) return true;
    if (cp == 1236) return true;
    if (cp == 1238) return true;
    if (cp == 1240) return true;
    if (cp == 1242) return true;
    if (cp == 1244) return true;
    if (cp == 1246) return true;
    if (cp == 1248) return true;
    if (cp == 1250) return true;
    if (cp == 1252) return true;
    if (cp == 1254) return true;
    if (cp == 1256) return true;
    if (cp == 1258) return true;
    if (cp == 1260) return true;
    if (cp == 1262) return true;
    if (cp == 1264) return true;
    if (cp == 1266) return true;
    if (cp == 1268) return true;
    if (cp == 1270) return true;
    if (cp == 1272) return true;
    if (cp == 1274) return true;
    if (cp == 1276) return true;
    if (cp == 1278) return true;
    if (cp == 1280) return true;
    if (cp == 1282) return true;
    if (cp == 1284) return true;
    if (cp == 1286) return true;
    if (cp == 1288) return true;
    if (cp == 1290) return true;
    if (cp == 1292) return true;
    if (cp == 1294) return true;
    if (cp == 1296) return true;
    if (cp == 1298) return true;
    if (cp == 1300) return true;
    if (cp == 1302) return true;
    if (cp == 1304) return true;
    if (cp == 1306) return true;
    if (cp == 1308) return true;
    if (cp == 1310) return true;
    if (cp == 1312) return true;
    if (cp == 1314) return true;
    if (cp == 1316) return true;
    if (cp == 1318) return true;
    if (cp == 1320) return true;
    if (cp == 1322) return true;
    if (cp == 1324) return true;
    if (cp == 1326) return true;
    if (cp >= 1329 && cp <= 1366) return true;
    if (cp >= 4256 && cp <= 4293) return true;
    if (cp == 4295) return true;
    if (cp == 4301) return true;
    if (cp >= 5024 && cp <= 5109) return true;
    if (cp == 7680) return true;
    if (cp == 7682) return true;
    if (cp == 7684) return true;
    if (cp == 7686) return true;
    if (cp == 7688) return true;
    if (cp == 7690) return true;
    if (cp == 7692) return true;
    if (cp == 7694) return true;
    if (cp == 7696) return true;
    if (cp == 7698) return true;
    if (cp == 7700) return true;
    if (cp == 7702) return true;
    if (cp == 7704) return true;
    if (cp == 7706) return true;
    if (cp == 7708) return true;
    if (cp == 7710) return true;
    if (cp == 7712) return true;
    if (cp == 7714) return true;
    if (cp == 7716) return true;
    if (cp == 7718) return true;
    if (cp == 7720) return true;
    if (cp == 7722) return true;
    if (cp == 7724) return true;
    if (cp == 7726) return true;
    if (cp == 7728) return true;
    if (cp == 7730) return true;
    if (cp == 7732) return true;
    if (cp == 7734) return true;
    if (cp == 7736) return true;
    if (cp == 7738) return true;
    if (cp == 7740) return true;
    if (cp == 7742) return true;
    if (cp == 7744) return true;
    if (cp == 7746) return true;
    if (cp == 7748) return true;
    if (cp == 7750) return true;
    if (cp == 7752) return true;
    if (cp == 7754) return true;
    if (cp == 7756) return true;
    if (cp == 7758) return true;
    if (cp == 7760) return true;
    if (cp == 7762) return true;
    if (cp == 7764) return true;
    if (cp == 7766) return true;
    if (cp == 7768) return true;
    if (cp == 7770) return true;
    if (cp == 7772) return true;
    if (cp == 7774) return true;
    if (cp == 7776) return true;
    if (cp == 7778) return true;
    if (cp == 7780) return true;
    if (cp == 7782) return true;
    if (cp == 7784) return true;
    if (cp == 7786) return true;
    if (cp == 7788) return true;
    if (cp == 7790) return true;
    if (cp == 7792) return true;
    if (cp == 7794) return true;
    if (cp == 7796) return true;
    if (cp == 7798) return true;
    if (cp == 7800) return true;
    if (cp == 7802) return true;
    if (cp == 7804) return true;
    if (cp == 7806) return true;
    if (cp == 7808) return true;
    if (cp == 7810) return true;
    if (cp == 7812) return true;
    if (cp == 7814) return true;
    if (cp == 7816) return true;
    if (cp == 7818) return true;
    if (cp == 7820) return true;
    if (cp == 7822) return true;
    if (cp == 7824) return true;
    if (cp == 7826) return true;
    if (cp == 7828) return true;
    if (cp == 7838) return true;
    if (cp == 7840) return true;
    if (cp == 7842) return true;
    if (cp == 7844) return true;
    if (cp == 7846) return true;
    if (cp == 7848) return true;
    if (cp == 7850) return true;
    if (cp == 7852) return true;
    if (cp == 7854) return true;
    if (cp == 7856) return true;
    if (cp == 7858) return true;
    if (cp == 7860) return true;
    if (cp == 7862) return true;
    if (cp == 7864) return true;
    if (cp == 7866) return true;
    if (cp == 7868) return true;
    if (cp == 7870) return true;
    if (cp == 7872) return true;
    if (cp == 7874) return true;
    if (cp == 7876) return true;
    if (cp == 7878) return true;
    if (cp == 7880) return true;
    if (cp == 7882) return true;
    if (cp == 7884) return true;
    if (cp == 7886) return true;
    if (cp == 7888) return true;
    if (cp == 7890) return true;
    if (cp == 7892) return true;
    if (cp == 7894) return true;
    if (cp == 7896) return true;
    if (cp == 7898) return true;
    if (cp == 7900) return true;
    if (cp == 7902) return true;
    if (cp == 7904) return true;
    if (cp == 7906) return true;
    if (cp == 7908) return true;
    if (cp == 7910) return true;
    if (cp == 7912) return true;
    if (cp == 7914) return true;
    if (cp == 7916) return true;
    if (cp == 7918) return true;
    if (cp == 7920) return true;
    if (cp == 7922) return true;
    if (cp == 7924) return true;
    if (cp == 7926) return true;
    if (cp == 7928) return true;
    if (cp == 7930) return true;
    if (cp == 7932) return true;
    if (cp == 7934) return true;
    if (cp >= 7944 && cp <= 7951) return true;
    if (cp >= 7960 && cp <= 7965) return true;
    if (cp >= 7976 && cp <= 7983) return true;
    if (cp >= 7992 && cp <= 7999) return true;
    if (cp >= 8008 && cp <= 8013) return true;
    if (cp == 8025) return true;
    if (cp == 8027) return true;
    if (cp == 8029) return true;
    if (cp == 8031) return true;
    if (cp >= 8040 && cp <= 8047) return true;
    if (cp >= 8120 && cp <= 8123) return true;
    if (cp >= 8136 && cp <= 8139) return true;
    if (cp >= 8152 && cp <= 8155) return true;
    if (cp >= 8168 && cp <= 8172) return true;
    if (cp >= 8184 && cp <= 8187) return true;
    if (cp == 8450) return true;
    if (cp == 8455) return true;
    if (cp >= 8459 && cp <= 8461) return true;
    if (cp >= 8464 && cp <= 8466) return true;
    if (cp == 8469) return true;
    if (cp >= 8473 && cp <= 8477) return true;
    if (cp == 8484) return true;
    if (cp == 8486) return true;
    if (cp == 8488) return true;
    if (cp >= 8490 && cp <= 8493) return true;
    if (cp >= 8496 && cp <= 8499) return true;
    if (cp >= 8510 && cp <= 8511) return true;
    if (cp == 8517) return true;
    if (cp >= 8544 && cp <= 8559) return true;
    if (cp == 8579) return true;
    if (cp >= 9398 && cp <= 9423) return true;
    if (cp >= 11264 && cp <= 11310) return true;
    if (cp == 11360) return true;
    if (cp >= 11362 && cp <= 11364) return true;
    if (cp == 11367) return true;
    if (cp == 11369) return true;
    if (cp == 11371) return true;
    if (cp >= 11373 && cp <= 11376) return true;
    if (cp == 11378) return true;
    if (cp == 11381) return true;
    if (cp >= 11390 && cp <= 11392) return true;
    if (cp == 11394) return true;
    if (cp == 11396) return true;
    if (cp == 11398) return true;
    if (cp == 11400) return true;
    if (cp == 11402) return true;
    if (cp == 11404) return true;
    if (cp == 11406) return true;
    if (cp == 11408) return true;
    if (cp == 11410) return true;
    if (cp == 11412) return true;
    if (cp == 11414) return true;
    if (cp == 11416) return true;
    if (cp == 11418) return true;
    if (cp == 11420) return true;
    if (cp == 11422) return true;
    if (cp == 11424) return true;
    if (cp == 11426) return true;
    if (cp == 11428) return true;
    if (cp == 11430) return true;
    if (cp == 11432) return true;
    if (cp == 11434) return true;
    if (cp == 11436) return true;
    if (cp == 11438) return true;
    if (cp == 11440) return true;
    if (cp == 11442) return true;
    if (cp == 11444) return true;
    if (cp == 11446) return true;
    if (cp == 11448) return true;
    if (cp == 11450) return true;
    if (cp == 11452) return true;
    if (cp == 11454) return true;
    if (cp == 11456) return true;
    if (cp == 11458) return true;
    if (cp == 11460) return true;
    if (cp == 11462) return true;
    if (cp == 11464) return true;
    if (cp == 11466) return true;
    if (cp == 11468) return true;
    if (cp == 11470) return true;
    if (cp == 11472) return true;
    if (cp == 11474) return true;
    if (cp == 11476) return true;
    if (cp == 11478) return true;
    if (cp == 11480) return true;
    if (cp == 11482) return true;
    if (cp == 11484) return true;
    if (cp == 11486) return true;
    if (cp == 11488) return true;
    if (cp == 11490) return true;
    if (cp == 11499) return true;
    if (cp == 11501) return true;
    if (cp == 11506) return true;
    if (cp == 42560) return true;
    if (cp == 42562) return true;
    if (cp == 42564) return true;
    if (cp == 42566) return true;
    if (cp == 42568) return true;
    if (cp == 42570) return true;
    if (cp == 42572) return true;
    if (cp == 42574) return true;
    if (cp == 42576) return true;
    if (cp == 42578) return true;
    if (cp == 42580) return true;
    if (cp == 42582) return true;
    if (cp == 42584) return true;
    if (cp == 42586) return true;
    if (cp == 42588) return true;
    if (cp == 42590) return true;
    if (cp == 42592) return true;
    if (cp == 42594) return true;
    if (cp == 42596) return true;
    if (cp == 42598) return true;
    if (cp == 42600) return true;
    if (cp == 42602) return true;
    if (cp == 42604) return true;
    if (cp == 42624) return true;
    if (cp == 42626) return true;
    if (cp == 42628) return true;
    if (cp == 42630) return true;
    if (cp == 42632) return true;
    if (cp == 42634) return true;
    if (cp == 42636) return true;
    if (cp == 42638) return true;
    if (cp == 42640) return true;
    if (cp == 42642) return true;
    if (cp == 42644) return true;
    if (cp == 42646) return true;
    if (cp == 42648) return true;
    if (cp == 42650) return true;
    if (cp == 42786) return true;
    if (cp == 42788) return true;
    if (cp == 42790) return true;
    if (cp == 42792) return true;
    if (cp == 42794) return true;
    if (cp == 42796) return true;
    if (cp == 42798) return true;
    if (cp == 42802) return true;
    if (cp == 42804) return true;
    if (cp == 42806) return true;
    if (cp == 42808) return true;
    if (cp == 42810) return true;
    if (cp == 42812) return true;
    if (cp == 42814) return true;
    if (cp == 42816) return true;
    if (cp == 42818) return true;
    if (cp == 42820) return true;
    if (cp == 42822) return true;
    if (cp == 42824) return true;
    if (cp == 42826) return true;
    if (cp == 42828) return true;
    if (cp == 42830) return true;
    if (cp == 42832) return true;
    if (cp == 42834) return true;
    if (cp == 42836) return true;
    if (cp == 42838) return true;
    if (cp == 42840) return true;
    if (cp == 42842) return true;
    if (cp == 42844) return true;
    if (cp == 42846) return true;
    if (cp == 42848) return true;
    if (cp == 42850) return true;
    if (cp == 42852) return true;
    if (cp == 42854) return true;
    if (cp == 42856) return true;
    if (cp == 42858) return true;
    if (cp == 42860) return true;
    if (cp == 42862) return true;
    if (cp == 42873) return true;
    if (cp == 42875) return true;
    if (cp >= 42877 && cp <= 42878) return true;
    if (cp == 42880) return true;
    if (cp == 42882) return true;
    if (cp == 42884) return true;
    if (cp == 42886) return true;
    if (cp == 42891) return true;
    if (cp == 42893) return true;
    if (cp == 42896) return true;
    if (cp == 42898) return true;
    if (cp == 42902) return true;
    if (cp == 42904) return true;
    if (cp == 42906) return true;
    if (cp == 42908) return true;
    if (cp == 42910) return true;
    if (cp == 42912) return true;
    if (cp == 42914) return true;
    if (cp == 42916) return true;
    if (cp == 42918) return true;
    if (cp == 42920) return true;
    if (cp >= 42922 && cp <= 42926) return true;
    if (cp >= 42928 && cp <= 42932) return true;
    if (cp == 42934) return true;
    if (cp >= 65313 && cp <= 65338) return true;
    if (cp >= 66560 && cp <= 66599) return true;
    if (cp >= 66736 && cp <= 66771) return true;
    if (cp >= 68736 && cp <= 68786) return true;
    if (cp >= 71840 && cp <= 71871) return true;
    if (cp >= 119808 && cp <= 119833) return true;
    if (cp >= 119860 && cp <= 119885) return true;
    if (cp >= 119912 && cp <= 119937) return true;
    if (cp == 119964) return true;
    if (cp >= 119966 && cp <= 119967) return true;
    if (cp == 119970) return true;
    if (cp >= 119973 && cp <= 119974) return true;
    if (cp >= 119977 && cp <= 119980) return true;
    if (cp >= 119982 && cp <= 119989) return true;
    if (cp >= 120016 && cp <= 120041) return true;
    if (cp >= 120068 && cp <= 120069) return true;
    if (cp >= 120071 && cp <= 120074) return true;
    if (cp >= 120077 && cp <= 120084) return true;
    if (cp >= 120086 && cp <= 120092) return true;
    if (cp >= 120120 && cp <= 120121) return true;
    if (cp >= 120123 && cp <= 120126) return true;
    if (cp >= 120128 && cp <= 120132) return true;
    if (cp == 120134) return true;
    if (cp >= 120138 && cp <= 120144) return true;
    if (cp >= 120172 && cp <= 120197) return true;
    if (cp >= 120224 && cp <= 120249) return true;
    if (cp >= 120276 && cp <= 120301) return true;
    if (cp >= 120328 && cp <= 120353) return true;
    if (cp >= 120380 && cp <= 120405) return true;
    if (cp >= 120432 && cp <= 120457) return true;
    if (cp >= 120488 && cp <= 120512) return true;
    if (cp >= 120546 && cp <= 120570) return true;
    if (cp >= 120604 && cp <= 120628) return true;
    if (cp >= 120662 && cp <= 120686) return true;
    if (cp >= 120720 && cp <= 120744) return true;
    if (cp == 120778) return true;
    if (cp >= 125184 && cp <= 125217) return true;
    if (cp >= 127280 && cp <= 127305) return true;
    if (cp >= 127312 && cp <= 127337) return true;
    if (cp >= 127344 && cp <= 127369) return true;

    return false;
}

function isAlpha(c) {
    var cp = ord(c);
    
    if (cp >= 65 && cp <= 90) return true;
    if (cp >= 97 && cp <= 122) return true;
    if (cp == 170) return true;
    if (cp == 181) return true;
    if (cp == 186) return true;
    if (cp >= 192 && cp <= 214) return true;
    if (cp >= 216 && cp <= 246) return true;
    if (cp >= 248 && cp <= 705) return true;
    if (cp >= 710 && cp <= 721) return true;
    if (cp >= 736 && cp <= 740) return true;
    if (cp == 748) return true;
    if (cp == 750) return true;
    if (cp == 837) return true;
    if (cp >= 880 && cp <= 884) return true;
    if (cp >= 886 && cp <= 887) return true;
    if (cp >= 890 && cp <= 893) return true;
    if (cp == 895) return true;
    if (cp == 902) return true;
    if (cp >= 904 && cp <= 906) return true;
    if (cp == 908) return true;
    if (cp >= 910 && cp <= 929) return true;
    if (cp >= 931 && cp <= 1013) return true;
    if (cp >= 1015 && cp <= 1153) return true;
    if (cp >= 1162 && cp <= 1327) return true;
    if (cp >= 1329 && cp <= 1366) return true;
    if (cp == 1369) return true;
    if (cp >= 1377 && cp <= 1415) return true;
    if (cp >= 1456 && cp <= 1469) return true;
    if (cp == 1471) return true;
    if (cp >= 1473 && cp <= 1474) return true;
    if (cp >= 1476 && cp <= 1477) return true;
    if (cp == 1479) return true;
    if (cp >= 1488 && cp <= 1514) return true;
    if (cp >= 1520 && cp <= 1522) return true;
    if (cp >= 1552 && cp <= 1562) return true;
    if (cp >= 1568 && cp <= 1623) return true;
    if (cp >= 1625 && cp <= 1631) return true;
    if (cp >= 1646 && cp <= 1747) return true;
    if (cp >= 1749 && cp <= 1756) return true;
    if (cp >= 1761 && cp <= 1768) return true;
    if (cp >= 1773 && cp <= 1775) return true;
    if (cp >= 1786 && cp <= 1788) return true;
    if (cp == 1791) return true;
    if (cp >= 1808 && cp <= 1855) return true;
    if (cp >= 1869 && cp <= 1969) return true;
    if (cp >= 1994 && cp <= 2026) return true;
    if (cp >= 2036 && cp <= 2037) return true;
    if (cp == 2042) return true;
    if (cp >= 2048 && cp <= 2071) return true;
    if (cp >= 2074 && cp <= 2092) return true;
    if (cp >= 2112 && cp <= 2136) return true;
    if (cp >= 2144 && cp <= 2154) return true;
    if (cp >= 2208 && cp <= 2228) return true;
    if (cp >= 2230 && cp <= 2237) return true;
    if (cp >= 2260 && cp <= 2271) return true;
    if (cp >= 2275 && cp <= 2281) return true;
    if (cp >= 2288 && cp <= 2363) return true;
    if (cp >= 2365 && cp <= 2380) return true;
    if (cp >= 2382 && cp <= 2384) return true;
    if (cp >= 2389 && cp <= 2403) return true;
    if (cp >= 2417 && cp <= 2435) return true;
    if (cp >= 2437 && cp <= 2444) return true;
    if (cp >= 2447 && cp <= 2448) return true;
    if (cp >= 2451 && cp <= 2472) return true;
    if (cp >= 2474 && cp <= 2480) return true;
    if (cp == 2482) return true;
    if (cp >= 2486 && cp <= 2489) return true;
    if (cp >= 2493 && cp <= 2500) return true;
    if (cp >= 2503 && cp <= 2504) return true;
    if (cp >= 2507 && cp <= 2508) return true;
    if (cp == 2510) return true;
    if (cp == 2519) return true;
    if (cp >= 2524 && cp <= 2525) return true;
    if (cp >= 2527 && cp <= 2531) return true;
    if (cp >= 2544 && cp <= 2545) return true;
    if (cp == 2556) return true;
    if (cp >= 2561 && cp <= 2563) return true;
    if (cp >= 2565 && cp <= 2570) return true;
    if (cp >= 2575 && cp <= 2576) return true;
    if (cp >= 2579 && cp <= 2600) return true;
    if (cp >= 2602 && cp <= 2608) return true;
    if (cp >= 2610 && cp <= 2611) return true;
    if (cp >= 2613 && cp <= 2614) return true;
    if (cp >= 2616 && cp <= 2617) return true;
    if (cp >= 2622 && cp <= 2626) return true;
    if (cp >= 2631 && cp <= 2632) return true;
    if (cp >= 2635 && cp <= 2636) return true;
    if (cp == 2641) return true;
    if (cp >= 2649 && cp <= 2652) return true;
    if (cp == 2654) return true;
    if (cp >= 2672 && cp <= 2677) return true;
    if (cp >= 2689 && cp <= 2691) return true;
    if (cp >= 2693 && cp <= 2701) return true;
    if (cp >= 2703 && cp <= 2705) return true;
    if (cp >= 2707 && cp <= 2728) return true;
    if (cp >= 2730 && cp <= 2736) return true;
    if (cp >= 2738 && cp <= 2739) return true;
    if (cp >= 2741 && cp <= 2745) return true;
    if (cp >= 2749 && cp <= 2757) return true;
    if (cp >= 2759 && cp <= 2761) return true;
    if (cp >= 2763 && cp <= 2764) return true;
    if (cp == 2768) return true;
    if (cp >= 2784 && cp <= 2787) return true;
    if (cp >= 2809 && cp <= 2812) return true;
    if (cp >= 2817 && cp <= 2819) return true;
    if (cp >= 2821 && cp <= 2828) return true;
    if (cp >= 2831 && cp <= 2832) return true;
    if (cp >= 2835 && cp <= 2856) return true;
    if (cp >= 2858 && cp <= 2864) return true;
    if (cp >= 2866 && cp <= 2867) return true;
    if (cp >= 2869 && cp <= 2873) return true;
    if (cp >= 2877 && cp <= 2884) return true;
    if (cp >= 2887 && cp <= 2888) return true;
    if (cp >= 2891 && cp <= 2892) return true;
    if (cp >= 2902 && cp <= 2903) return true;
    if (cp >= 2908 && cp <= 2909) return true;
    if (cp >= 2911 && cp <= 2915) return true;
    if (cp == 2929) return true;
    if (cp >= 2946 && cp <= 2947) return true;
    if (cp >= 2949 && cp <= 2954) return true;
    if (cp >= 2958 && cp <= 2960) return true;
    if (cp >= 2962 && cp <= 2965) return true;
    if (cp >= 2969 && cp <= 2970) return true;
    if (cp == 2972) return true;
    if (cp >= 2974 && cp <= 2975) return true;
    if (cp >= 2979 && cp <= 2980) return true;
    if (cp >= 2984 && cp <= 2986) return true;
    if (cp >= 2990 && cp <= 3001) return true;
    if (cp >= 3006 && cp <= 3010) return true;
    if (cp >= 3014 && cp <= 3016) return true;
    if (cp >= 3018 && cp <= 3020) return true;
    if (cp == 3024) return true;
    if (cp == 3031) return true;
    if (cp >= 3072 && cp <= 3075) return true;
    if (cp >= 3077 && cp <= 3084) return true;
    if (cp >= 3086 && cp <= 3088) return true;
    if (cp >= 3090 && cp <= 3112) return true;
    if (cp >= 3114 && cp <= 3129) return true;
    if (cp >= 3133 && cp <= 3140) return true;
    if (cp >= 3142 && cp <= 3144) return true;
    if (cp >= 3146 && cp <= 3148) return true;
    if (cp >= 3157 && cp <= 3158) return true;
    if (cp >= 3160 && cp <= 3162) return true;
    if (cp >= 3168 && cp <= 3171) return true;
    if (cp >= 3200 && cp <= 3203) return true;
    if (cp >= 3205 && cp <= 3212) return true;
    if (cp >= 3214 && cp <= 3216) return true;
    if (cp >= 3218 && cp <= 3240) return true;
    if (cp >= 3242 && cp <= 3251) return true;
    if (cp >= 3253 && cp <= 3257) return true;
    if (cp >= 3261 && cp <= 3268) return true;
    if (cp >= 3270 && cp <= 3272) return true;
    if (cp >= 3274 && cp <= 3276) return true;
    if (cp >= 3285 && cp <= 3286) return true;
    if (cp == 3294) return true;
    if (cp >= 3296 && cp <= 3299) return true;
    if (cp >= 3313 && cp <= 3314) return true;
    if (cp >= 3328 && cp <= 3331) return true;
    if (cp >= 3333 && cp <= 3340) return true;
    if (cp >= 3342 && cp <= 3344) return true;
    if (cp >= 3346 && cp <= 3386) return true;
    if (cp >= 3389 && cp <= 3396) return true;
    if (cp >= 3398 && cp <= 3400) return true;
    if (cp >= 3402 && cp <= 3404) return true;
    if (cp == 3406) return true;
    if (cp >= 3412 && cp <= 3415) return true;
    if (cp >= 3423 && cp <= 3427) return true;
    if (cp >= 3450 && cp <= 3455) return true;
    if (cp >= 3458 && cp <= 3459) return true;
    if (cp >= 3461 && cp <= 3478) return true;
    if (cp >= 3482 && cp <= 3505) return true;
    if (cp >= 3507 && cp <= 3515) return true;
    if (cp == 3517) return true;
    if (cp >= 3520 && cp <= 3526) return true;
    if (cp >= 3535 && cp <= 3540) return true;
    if (cp == 3542) return true;
    if (cp >= 3544 && cp <= 3551) return true;
    if (cp >= 3570 && cp <= 3571) return true;
    if (cp >= 3585 && cp <= 3642) return true;
    if (cp >= 3648 && cp <= 3654) return true;
    if (cp == 3661) return true;
    if (cp >= 3713 && cp <= 3714) return true;
    if (cp == 3716) return true;
    if (cp >= 3719 && cp <= 3720) return true;
    if (cp == 3722) return true;
    if (cp == 3725) return true;
    if (cp >= 3732 && cp <= 3735) return true;
    if (cp >= 3737 && cp <= 3743) return true;
    if (cp >= 3745 && cp <= 3747) return true;
    if (cp == 3749) return true;
    if (cp == 3751) return true;
    if (cp >= 3754 && cp <= 3755) return true;
    if (cp >= 3757 && cp <= 3769) return true;
    if (cp >= 3771 && cp <= 3773) return true;
    if (cp >= 3776 && cp <= 3780) return true;
    if (cp == 3782) return true;
    if (cp == 3789) return true;
    if (cp >= 3804 && cp <= 3807) return true;
    if (cp == 3840) return true;
    if (cp >= 3904 && cp <= 3911) return true;
    if (cp >= 3913 && cp <= 3948) return true;
    if (cp >= 3953 && cp <= 3969) return true;
    if (cp >= 3976 && cp <= 3991) return true;
    if (cp >= 3993 && cp <= 4028) return true;
    if (cp >= 4096 && cp <= 4150) return true;
    if (cp == 4152) return true;
    if (cp >= 4155 && cp <= 4159) return true;
    if (cp >= 4176 && cp <= 4194) return true;
    if (cp >= 4197 && cp <= 4200) return true;
    if (cp >= 4206 && cp <= 4230) return true;
    if (cp == 4238) return true;
    if (cp >= 4252 && cp <= 4253) return true;
    if (cp >= 4256 && cp <= 4293) return true;
    if (cp == 4295) return true;
    if (cp == 4301) return true;
    if (cp >= 4304 && cp <= 4346) return true;
    if (cp >= 4348 && cp <= 4680) return true;
    if (cp >= 4682 && cp <= 4685) return true;
    if (cp >= 4688 && cp <= 4694) return true;
    if (cp == 4696) return true;
    if (cp >= 4698 && cp <= 4701) return true;
    if (cp >= 4704 && cp <= 4744) return true;
    if (cp >= 4746 && cp <= 4749) return true;
    if (cp >= 4752 && cp <= 4784) return true;
    if (cp >= 4786 && cp <= 4789) return true;
    if (cp >= 4792 && cp <= 4798) return true;
    if (cp == 4800) return true;
    if (cp >= 4802 && cp <= 4805) return true;
    if (cp >= 4808 && cp <= 4822) return true;
    if (cp >= 4824 && cp <= 4880) return true;
    if (cp >= 4882 && cp <= 4885) return true;
    if (cp >= 4888 && cp <= 4954) return true;
    if (cp == 4959) return true;
    if (cp >= 4992 && cp <= 5007) return true;
    if (cp >= 5024 && cp <= 5109) return true;
    if (cp >= 5112 && cp <= 5117) return true;
    if (cp >= 5121 && cp <= 5740) return true;
    if (cp >= 5743 && cp <= 5759) return true;
    if (cp >= 5761 && cp <= 5786) return true;
    if (cp >= 5792 && cp <= 5866) return true;
    if (cp >= 5870 && cp <= 5880) return true;
    if (cp >= 5888 && cp <= 5900) return true;
    if (cp >= 5902 && cp <= 5907) return true;
    if (cp >= 5920 && cp <= 5939) return true;
    if (cp >= 5952 && cp <= 5971) return true;
    if (cp >= 5984 && cp <= 5996) return true;
    if (cp >= 5998 && cp <= 6000) return true;
    if (cp >= 6002 && cp <= 6003) return true;
    if (cp >= 6016 && cp <= 6067) return true;
    if (cp >= 6070 && cp <= 6088) return true;
    if (cp == 6103) return true;
    if (cp == 6108) return true;
    if (cp >= 6176 && cp <= 6263) return true;
    if (cp >= 6272 && cp <= 6314) return true;
    if (cp >= 6320 && cp <= 6389) return true;
    if (cp >= 6400 && cp <= 6430) return true;
    if (cp >= 6432 && cp <= 6443) return true;
    if (cp >= 6448 && cp <= 6456) return true;
    if (cp >= 6480 && cp <= 6509) return true;
    if (cp >= 6512 && cp <= 6516) return true;
    if (cp >= 6528 && cp <= 6571) return true;
    if (cp >= 6576 && cp <= 6601) return true;
    if (cp >= 6656 && cp <= 6683) return true;
    if (cp >= 6688 && cp <= 6750) return true;
    if (cp >= 6753 && cp <= 6772) return true;
    if (cp == 6823) return true;
    if (cp >= 6912 && cp <= 6963) return true;
    if (cp >= 6965 && cp <= 6979) return true;
    if (cp >= 6981 && cp <= 6987) return true;
    if (cp >= 7040 && cp <= 7081) return true;
    if (cp >= 7084 && cp <= 7087) return true;
    if (cp >= 7098 && cp <= 7141) return true;
    if (cp >= 7143 && cp <= 7153) return true;
    if (cp >= 7168 && cp <= 7221) return true;
    if (cp >= 7245 && cp <= 7247) return true;
    if (cp >= 7258 && cp <= 7293) return true;
    if (cp >= 7296 && cp <= 7304) return true;
    if (cp >= 7401 && cp <= 7404) return true;
    if (cp >= 7406 && cp <= 7411) return true;
    if (cp >= 7413 && cp <= 7414) return true;
    if (cp >= 7424 && cp <= 7615) return true;
    if (cp >= 7655 && cp <= 7668) return true;
    if (cp >= 7680 && cp <= 7957) return true;
    if (cp >= 7960 && cp <= 7965) return true;
    if (cp >= 7968 && cp <= 8005) return true;
    if (cp >= 8008 && cp <= 8013) return true;
    if (cp >= 8016 && cp <= 8023) return true;
    if (cp == 8025) return true;
    if (cp == 8027) return true;
    if (cp == 8029) return true;
    if (cp >= 8031 && cp <= 8061) return true;
    if (cp >= 8064 && cp <= 8116) return true;
    if (cp >= 8118 && cp <= 8124) return true;
    if (cp == 8126) return true;
    if (cp >= 8130 && cp <= 8132) return true;
    if (cp >= 8134 && cp <= 8140) return true;
    if (cp >= 8144 && cp <= 8147) return true;
    if (cp >= 8150 && cp <= 8155) return true;
    if (cp >= 8160 && cp <= 8172) return true;
    if (cp >= 8178 && cp <= 8180) return true;
    if (cp >= 8182 && cp <= 8188) return true;
    if (cp == 8305) return true;
    if (cp == 8319) return true;
    if (cp >= 8336 && cp <= 8348) return true;
    if (cp == 8450) return true;
    if (cp == 8455) return true;
    if (cp >= 8458 && cp <= 8467) return true;
    if (cp == 8469) return true;
    if (cp >= 8473 && cp <= 8477) return true;
    if (cp == 8484) return true;
    if (cp == 8486) return true;
    if (cp == 8488) return true;
    if (cp >= 8490 && cp <= 8493) return true;
    if (cp >= 8495 && cp <= 8505) return true;
    if (cp >= 8508 && cp <= 8511) return true;
    if (cp >= 8517 && cp <= 8521) return true;
    if (cp == 8526) return true;
    if (cp >= 8544 && cp <= 8584) return true;
    if (cp >= 9398 && cp <= 9449) return true;
    if (cp >= 11264 && cp <= 11310) return true;
    if (cp >= 11312 && cp <= 11358) return true;
    if (cp >= 11360 && cp <= 11492) return true;
    if (cp >= 11499 && cp <= 11502) return true;
    if (cp >= 11506 && cp <= 11507) return true;
    if (cp >= 11520 && cp <= 11557) return true;
    if (cp == 11559) return true;
    if (cp == 11565) return true;
    if (cp >= 11568 && cp <= 11623) return true;
    if (cp == 11631) return true;
    if (cp >= 11648 && cp <= 11670) return true;
    if (cp >= 11680 && cp <= 11686) return true;
    if (cp >= 11688 && cp <= 11694) return true;
    if (cp >= 11696 && cp <= 11702) return true;
    if (cp >= 11704 && cp <= 11710) return true;
    if (cp >= 11712 && cp <= 11718) return true;
    if (cp >= 11720 && cp <= 11726) return true;
    if (cp >= 11728 && cp <= 11734) return true;
    if (cp >= 11736 && cp <= 11742) return true;
    if (cp >= 11744 && cp <= 11775) return true;
    if (cp == 11823) return true;
    if (cp >= 12293 && cp <= 12295) return true;
    if (cp >= 12321 && cp <= 12329) return true;
    if (cp >= 12337 && cp <= 12341) return true;
    if (cp >= 12344 && cp <= 12348) return true;
    if (cp >= 12353 && cp <= 12438) return true;
    if (cp >= 12445 && cp <= 12447) return true;
    if (cp >= 12449 && cp <= 12538) return true;
    if (cp >= 12540 && cp <= 12543) return true;
    if (cp >= 12549 && cp <= 12590) return true;
    if (cp >= 12593 && cp <= 12686) return true;
    if (cp >= 12704 && cp <= 12730) return true;
    if (cp >= 12784 && cp <= 12799) return true;
    if (cp >= 13312 && cp <= 19893) return true;
    if (cp >= 19968 && cp <= 40938) return true;
    if (cp >= 40960 && cp <= 42124) return true;
    if (cp >= 42192 && cp <= 42237) return true;
    if (cp >= 42240 && cp <= 42508) return true;
    if (cp >= 42512 && cp <= 42527) return true;
    if (cp >= 42538 && cp <= 42539) return true;
    if (cp >= 42560 && cp <= 42606) return true;
    if (cp >= 42612 && cp <= 42619) return true;
    if (cp >= 42623 && cp <= 42735) return true;
    if (cp >= 42775 && cp <= 42783) return true;
    if (cp >= 42786 && cp <= 42888) return true;
    if (cp >= 42891 && cp <= 42926) return true;
    if (cp >= 42928 && cp <= 42935) return true;
    if (cp >= 42999 && cp <= 43009) return true;
    if (cp >= 43011 && cp <= 43013) return true;
    if (cp >= 43015 && cp <= 43018) return true;
    if (cp >= 43020 && cp <= 43047) return true;
    if (cp >= 43072 && cp <= 43123) return true;
    if (cp >= 43136 && cp <= 43203) return true;
    if (cp == 43205) return true;
    if (cp >= 43250 && cp <= 43255) return true;
    if (cp == 43259) return true;
    if (cp == 43261) return true;
    if (cp >= 43274 && cp <= 43306) return true;
    if (cp >= 43312 && cp <= 43346) return true;
    if (cp >= 43360 && cp <= 43388) return true;
    if (cp >= 43392 && cp <= 43442) return true;
    if (cp >= 43444 && cp <= 43455) return true;
    if (cp == 43471) return true;
    if (cp >= 43488 && cp <= 43492) return true;
    if (cp >= 43494 && cp <= 43503) return true;
    if (cp >= 43514 && cp <= 43518) return true;
    if (cp >= 43520 && cp <= 43574) return true;
    if (cp >= 43584 && cp <= 43597) return true;
    if (cp >= 43616 && cp <= 43638) return true;
    if (cp == 43642) return true;
    if (cp >= 43646 && cp <= 43710) return true;
    if (cp == 43712) return true;
    if (cp == 43714) return true;
    if (cp >= 43739 && cp <= 43741) return true;
    if (cp >= 43744 && cp <= 43759) return true;
    if (cp >= 43762 && cp <= 43765) return true;
    if (cp >= 43777 && cp <= 43782) return true;
    if (cp >= 43785 && cp <= 43790) return true;
    if (cp >= 43793 && cp <= 43798) return true;
    if (cp >= 43808 && cp <= 43814) return true;
    if (cp >= 43816 && cp <= 43822) return true;
    if (cp >= 43824 && cp <= 43866) return true;
    if (cp >= 43868 && cp <= 43877) return true;
    if (cp >= 43888 && cp <= 44010) return true;
    if (cp >= 44032 && cp <= 55203) return true;
    if (cp >= 55216 && cp <= 55238) return true;
    if (cp >= 55243 && cp <= 55291) return true;
    if (cp >= 63744 && cp <= 64109) return true;
    if (cp >= 64112 && cp <= 64217) return true;
    if (cp >= 64256 && cp <= 64262) return true;
    if (cp >= 64275 && cp <= 64279) return true;
    if (cp >= 64285 && cp <= 64296) return true;
    if (cp >= 64298 && cp <= 64310) return true;
    if (cp >= 64312 && cp <= 64316) return true;
    if (cp == 64318) return true;
    if (cp >= 64320 && cp <= 64321) return true;
    if (cp >= 64323 && cp <= 64324) return true;
    if (cp >= 64326 && cp <= 64433) return true;
    if (cp >= 64467 && cp <= 64829) return true;
    if (cp >= 64848 && cp <= 64911) return true;
    if (cp >= 64914 && cp <= 64967) return true;
    if (cp >= 65008 && cp <= 65019) return true;
    if (cp >= 65136 && cp <= 65140) return true;
    if (cp >= 65142 && cp <= 65276) return true;
    if (cp >= 65313 && cp <= 65338) return true;
    if (cp >= 65345 && cp <= 65370) return true;
    if (cp >= 65382 && cp <= 65470) return true;
    if (cp >= 65474 && cp <= 65479) return true;
    if (cp >= 65482 && cp <= 65487) return true;
    if (cp >= 65490 && cp <= 65495) return true;
    if (cp >= 65498 && cp <= 65500) return true;
    if (cp >= 65536 && cp <= 65547) return true;
    if (cp >= 65549 && cp <= 65574) return true;
    if (cp >= 65576 && cp <= 65594) return true;
    if (cp >= 65596 && cp <= 65597) return true;
    if (cp >= 65599 && cp <= 65613) return true;
    if (cp >= 65616 && cp <= 65629) return true;
    if (cp >= 65664 && cp <= 65786) return true;
    if (cp >= 65856 && cp <= 65908) return true;
    if (cp >= 66176 && cp <= 66204) return true;
    if (cp >= 66208 && cp <= 66256) return true;
    if (cp >= 66304 && cp <= 66335) return true;
    if (cp >= 66349 && cp <= 66378) return true;
    if (cp >= 66384 && cp <= 66426) return true;
    if (cp >= 66432 && cp <= 66461) return true;
    if (cp >= 66464 && cp <= 66499) return true;
    if (cp >= 66504 && cp <= 66511) return true;
    if (cp >= 66513 && cp <= 66517) return true;
    if (cp >= 66560 && cp <= 66717) return true;
    if (cp >= 66736 && cp <= 66771) return true;
    if (cp >= 66776 && cp <= 66811) return true;
    if (cp >= 66816 && cp <= 66855) return true;
    if (cp >= 66864 && cp <= 66915) return true;
    if (cp >= 67072 && cp <= 67382) return true;
    if (cp >= 67392 && cp <= 67413) return true;
    if (cp >= 67424 && cp <= 67431) return true;
    if (cp >= 67584 && cp <= 67589) return true;
    if (cp == 67592) return true;
    if (cp >= 67594 && cp <= 67637) return true;
    if (cp >= 67639 && cp <= 67640) return true;
    if (cp == 67644) return true;
    if (cp >= 67647 && cp <= 67669) return true;
    if (cp >= 67680 && cp <= 67702) return true;
    if (cp >= 67712 && cp <= 67742) return true;
    if (cp >= 67808 && cp <= 67826) return true;
    if (cp >= 67828 && cp <= 67829) return true;
    if (cp >= 67840 && cp <= 67861) return true;
    if (cp >= 67872 && cp <= 67897) return true;
    if (cp >= 67968 && cp <= 68023) return true;
    if (cp >= 68030 && cp <= 68031) return true;
    if (cp >= 68096 && cp <= 68099) return true;
    if (cp >= 68101 && cp <= 68102) return true;
    if (cp >= 68108 && cp <= 68115) return true;
    if (cp >= 68117 && cp <= 68119) return true;
    if (cp >= 68121 && cp <= 68147) return true;
    if (cp >= 68192 && cp <= 68220) return true;
    if (cp >= 68224 && cp <= 68252) return true;
    if (cp >= 68288 && cp <= 68295) return true;
    if (cp >= 68297 && cp <= 68324) return true;
    if (cp >= 68352 && cp <= 68405) return true;
    if (cp >= 68416 && cp <= 68437) return true;
    if (cp >= 68448 && cp <= 68466) return true;
    if (cp >= 68480 && cp <= 68497) return true;
    if (cp >= 68608 && cp <= 68680) return true;
    if (cp >= 68736 && cp <= 68786) return true;
    if (cp >= 68800 && cp <= 68850) return true;
    if (cp >= 69632 && cp <= 69701) return true;
    if (cp >= 69762 && cp <= 69816) return true;
    if (cp >= 69840 && cp <= 69864) return true;
    if (cp >= 69888 && cp <= 69938) return true;
    if (cp >= 69968 && cp <= 70002) return true;
    if (cp == 70006) return true;
    if (cp >= 70016 && cp <= 70079) return true;
    if (cp >= 70081 && cp <= 70084) return true;
    if (cp == 70106) return true;
    if (cp == 70108) return true;
    if (cp >= 70144 && cp <= 70161) return true;
    if (cp >= 70163 && cp <= 70196) return true;
    if (cp == 70199) return true;
    if (cp == 70206) return true;
    if (cp >= 70272 && cp <= 70278) return true;
    if (cp == 70280) return true;
    if (cp >= 70282 && cp <= 70285) return true;
    if (cp >= 70287 && cp <= 70301) return true;
    if (cp >= 70303 && cp <= 70312) return true;
    if (cp >= 70320 && cp <= 70376) return true;
    if (cp >= 70400 && cp <= 70403) return true;
    if (cp >= 70405 && cp <= 70412) return true;
    if (cp >= 70415 && cp <= 70416) return true;
    if (cp >= 70419 && cp <= 70440) return true;
    if (cp >= 70442 && cp <= 70448) return true;
    if (cp >= 70450 && cp <= 70451) return true;
    if (cp >= 70453 && cp <= 70457) return true;
    if (cp >= 70461 && cp <= 70468) return true;
    if (cp >= 70471 && cp <= 70472) return true;
    if (cp >= 70475 && cp <= 70476) return true;
    if (cp == 70480) return true;
    if (cp == 70487) return true;
    if (cp >= 70493 && cp <= 70499) return true;
    if (cp >= 70656 && cp <= 70721) return true;
    if (cp >= 70723 && cp <= 70725) return true;
    if (cp >= 70727 && cp <= 70730) return true;
    if (cp >= 70784 && cp <= 70849) return true;
    if (cp >= 70852 && cp <= 70853) return true;
    if (cp == 70855) return true;
    if (cp >= 71040 && cp <= 71093) return true;
    if (cp >= 71096 && cp <= 71102) return true;
    if (cp >= 71128 && cp <= 71133) return true;
    if (cp >= 71168 && cp <= 71230) return true;
    if (cp == 71232) return true;
    if (cp == 71236) return true;
    if (cp >= 71296 && cp <= 71349) return true;
    if (cp >= 71424 && cp <= 71449) return true;
    if (cp >= 71453 && cp <= 71466) return true;
    if (cp >= 71840 && cp <= 71903) return true;
    if (cp == 71935) return true;
    if (cp >= 72192 && cp <= 72242) return true;
    if (cp >= 72245 && cp <= 72254) return true;
    if (cp >= 72272 && cp <= 72323) return true;
    if (cp >= 72326 && cp <= 72343) return true;
    if (cp >= 72384 && cp <= 72440) return true;
    if (cp >= 72704 && cp <= 72712) return true;
    if (cp >= 72714 && cp <= 72758) return true;
    if (cp >= 72760 && cp <= 72766) return true;
    if (cp == 72768) return true;
    if (cp >= 72818 && cp <= 72847) return true;
    if (cp >= 72850 && cp <= 72871) return true;
    if (cp >= 72873 && cp <= 72886) return true;
    if (cp >= 72960 && cp <= 72966) return true;
    if (cp >= 72968 && cp <= 72969) return true;
    if (cp >= 72971 && cp <= 73014) return true;
    if (cp == 73018) return true;
    if (cp >= 73020 && cp <= 73021) return true;
    if (cp >= 73023 && cp <= 73025) return true;
    if (cp == 73027) return true;
    if (cp >= 73030 && cp <= 73031) return true;
    if (cp >= 73728 && cp <= 74649) return true;
    if (cp >= 74752 && cp <= 74862) return true;
    if (cp >= 74880 && cp <= 75075) return true;
    if (cp >= 77824 && cp <= 78894) return true;
    if (cp >= 82944 && cp <= 83526) return true;
    if (cp >= 92160 && cp <= 92728) return true;
    if (cp >= 92736 && cp <= 92766) return true;
    if (cp >= 92880 && cp <= 92909) return true;
    if (cp >= 92928 && cp <= 92982) return true;
    if (cp >= 92992 && cp <= 92995) return true;
    if (cp >= 93027 && cp <= 93047) return true;
    if (cp >= 93053 && cp <= 93071) return true;
    if (cp >= 93952 && cp <= 94020) return true;
    if (cp >= 94032 && cp <= 94078) return true;
    if (cp >= 94099 && cp <= 94111) return true;
    if (cp >= 94176 && cp <= 94177) return true;
    if (cp >= 94208 && cp <= 100332) return true;
    if (cp >= 100352 && cp <= 101106) return true;
    if (cp >= 110592 && cp <= 110878) return true;
    if (cp >= 110960 && cp <= 111355) return true;
    if (cp >= 113664 && cp <= 113770) return true;
    if (cp >= 113776 && cp <= 113788) return true;
    if (cp >= 113792 && cp <= 113800) return true;
    if (cp >= 113808 && cp <= 113817) return true;
    if (cp == 113822) return true;
    if (cp >= 119808 && cp <= 119892) return true;
    if (cp >= 119894 && cp <= 119964) return true;
    if (cp >= 119966 && cp <= 119967) return true;
    if (cp == 119970) return true;
    if (cp >= 119973 && cp <= 119974) return true;
    if (cp >= 119977 && cp <= 119980) return true;
    if (cp >= 119982 && cp <= 119993) return true;
    if (cp == 119995) return true;
    if (cp >= 119997 && cp <= 120003) return true;
    if (cp >= 120005 && cp <= 120069) return true;
    if (cp >= 120071 && cp <= 120074) return true;
    if (cp >= 120077 && cp <= 120084) return true;
    if (cp >= 120086 && cp <= 120092) return true;
    if (cp >= 120094 && cp <= 120121) return true;
    if (cp >= 120123 && cp <= 120126) return true;
    if (cp >= 120128 && cp <= 120132) return true;
    if (cp == 120134) return true;
    if (cp >= 120138 && cp <= 120144) return true;
    if (cp >= 120146 && cp <= 120485) return true;
    if (cp >= 120488 && cp <= 120512) return true;
    if (cp >= 120514 && cp <= 120538) return true;
    if (cp >= 120540 && cp <= 120570) return true;
    if (cp >= 120572 && cp <= 120596) return true;
    if (cp >= 120598 && cp <= 120628) return true;
    if (cp >= 120630 && cp <= 120654) return true;
    if (cp >= 120656 && cp <= 120686) return true;
    if (cp >= 120688 && cp <= 120712) return true;
    if (cp >= 120714 && cp <= 120744) return true;
    if (cp >= 120746 && cp <= 120770) return true;
    if (cp >= 120772 && cp <= 120779) return true;
    if (cp >= 122880 && cp <= 122886) return true;
    if (cp >= 122888 && cp <= 122904) return true;
    if (cp >= 122907 && cp <= 122913) return true;
    if (cp >= 122915 && cp <= 122916) return true;
    if (cp >= 122918 && cp <= 122922) return true;
    if (cp >= 124928 && cp <= 125124) return true;
    if (cp >= 125184 && cp <= 125251) return true;
    if (cp == 125255) return true;
    if (cp >= 126464 && cp <= 126467) return true;
    if (cp >= 126469 && cp <= 126495) return true;
    if (cp >= 126497 && cp <= 126498) return true;
    if (cp == 126500) return true;
    if (cp == 126503) return true;
    if (cp >= 126505 && cp <= 126514) return true;
    if (cp >= 126516 && cp <= 126519) return true;
    if (cp == 126521) return true;
    if (cp == 126523) return true;
    if (cp == 126530) return true;
    if (cp == 126535) return true;
    if (cp == 126537) return true;
    if (cp == 126539) return true;
    if (cp >= 126541 && cp <= 126543) return true;
    if (cp >= 126545 && cp <= 126546) return true;
    if (cp == 126548) return true;
    if (cp == 126551) return true;
    if (cp == 126553) return true;
    if (cp == 126555) return true;
    if (cp == 126557) return true;
    if (cp == 126559) return true;
    if (cp >= 126561 && cp <= 126562) return true;
    if (cp == 126564) return true;
    if (cp >= 126567 && cp <= 126570) return true;
    if (cp >= 126572 && cp <= 126578) return true;
    if (cp >= 126580 && cp <= 126583) return true;
    if (cp >= 126585 && cp <= 126588) return true;
    if (cp == 126590) return true;
    if (cp >= 126592 && cp <= 126601) return true;
    if (cp >= 126603 && cp <= 126619) return true;
    if (cp >= 126625 && cp <= 126627) return true;
    if (cp >= 126629 && cp <= 126633) return true;
    if (cp >= 126635 && cp <= 126651) return true;
    if (cp >= 127280 && cp <= 127305) return true;
    if (cp >= 127312 && cp <= 127337) return true;
    if (cp >= 127344 && cp <= 127369) return true;
    if (cp >= 131072 && cp <= 173782) return true;
    if (cp >= 173824 && cp <= 177972) return true;
    if (cp >= 177984 && cp <= 178205) return true;
    if (cp >= 178208 && cp <= 183969) return true;
    if (cp >= 183984 && cp <= 191456) return true;
    if (cp >= 194560 && cp <= 195101) return true;

    return false;
}

function isDigit(c) {
    var cp = ord(c);
    if (cp >= 48 && cp <= 57)
	return true;
    if (cp >= 1632 && cp <= 1641)
	return true;
    if (cp >= 1776 && cp <= 1785)
	return true;
    if (cp >= 1984 && cp <= 1993)
	return true;
    if (cp >= 2406 && cp <= 2415)
	return true;
    if (cp >= 2534 && cp <= 2543)
	return true;
    if (cp >= 2662 && cp <= 2671)
	return true;
    if (cp >= 2790 && cp <= 2799)
	return true;
    if (cp >= 2918 && cp <= 2927)
	return true;
    if (cp >= 3046 && cp <= 3055)
	return true;
    if (cp >= 3174 && cp <= 3183)
	return true;
    if (cp >= 3302 && cp <= 3311)
	return true;
    if (cp >= 3430 && cp <= 3439)
	return true;
    if (cp >= 3558 && cp <= 3567)
	return true;
    if (cp >= 3664 && cp <= 3673)
	return true;
    if (cp >= 3792 && cp <= 3801)
	return true;
    if (cp >= 3872 && cp <= 3881)
	return true;
    if (cp >= 4160 && cp <= 4169)
	return true;
    if (cp >= 4240 && cp <= 4249)
	return true;
    if (cp >= 6112 && cp <= 6121)
	return true;
    if (cp >= 6160 && cp <= 6169)
	return true;
    if (cp >= 6470 && cp <= 6479)
	return true;
    if (cp >= 6608 && cp <= 6617)
	return true;
    if (cp >= 6784 && cp <= 6793)
	return true;
    if (cp >= 6800 && cp <= 6809)
	return true;
    if (cp >= 6992 && cp <= 7001)
	return true;
    if (cp >= 7088 && cp <= 7097)
	return true;
    if (cp >= 7232 && cp <= 7241)
	return true;
    if (cp >= 7248 && cp <= 7257)
	return true;
    if (cp >= 42528 && cp <= 42537)
	return true;
    if (cp >= 43216 && cp <= 43225)
	return true;
    if (cp >= 43264 && cp <= 43273)
	return true;
    if (cp >= 43472 && cp <= 43481)
	return true;
    if (cp >= 43504 && cp <= 43513)
	return true;
    if (cp >= 43600 && cp <= 43609)
	return true;
    if (cp >= 44016 && cp <= 44025)
	return true;
    if (cp >= 65296 && cp <= 65305)
	return true;
    if (cp >= 66720 && cp <= 66729)
	return true;
    if (cp >= 69734 && cp <= 69743)
	return true;
    if (cp >= 69872 && cp <= 69881)
	return true;
    if (cp >= 69942 && cp <= 69951)
	return true;
    if (cp >= 70096 && cp <= 70105)
	return true;
    if (cp >= 70384 && cp <= 70393)
	return true;
    if (cp >= 70736 && cp <= 70745)
	return true;
    if (cp >= 70864 && cp <= 70873)
	return true;
    if (cp >= 71248 && cp <= 71257)
	return true;
    if (cp >= 71360 && cp <= 71369)
	return true;
    if (cp >= 71472 && cp <= 71481)
	return true;
    if (cp >= 71904 && cp <= 71913)
	return true;
    if (cp >= 72784 && cp <= 72793)
	return true;
    if (cp >= 73040 && cp <= 73049)
	return true;
    if (cp >= 92768 && cp <= 92777)
	return true;
    if (cp >= 93008 && cp <= 93017)
	return true;
    if (cp >= 120782 && cp <= 120831)
	return true;
    if (cp >= 125264 && cp <= 125273)
	return true;
    
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
    if (cp == 10)
	return true;
    if (cp == 13)
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

function isPrint(c) {
    var cp = ord(c);
    if (cp >= 32 && cp <= 126) return true;
    if (cp >= 160 && cp <= 887) return true;
    if (cp >= 890 && cp <= 895) return true;
    if (cp >= 900 && cp <= 906) return true;
    if (cp == 908) return true;
    if (cp >= 910 && cp <= 929) return true;
    if (cp >= 931 && cp <= 1327) return true;
    if (cp >= 1329 && cp <= 1366) return true;
    if (cp >= 1369 && cp <= 1375) return true;
    if (cp >= 1377 && cp <= 1415) return true;
    if (cp >= 1417 && cp <= 1418) return true;
    if (cp >= 1421 && cp <= 1423) return true;
    if (cp >= 1425 && cp <= 1479) return true;
    if (cp >= 1488 && cp <= 1514) return true;
    if (cp >= 1520 && cp <= 1524) return true;
    if (cp >= 1536 && cp <= 1564) return true;
    if (cp >= 1566 && cp <= 1805) return true;
    if (cp >= 1807 && cp <= 1866) return true;
    if (cp >= 1869 && cp <= 1969) return true;
    if (cp >= 1984 && cp <= 2042) return true;
    if (cp >= 2048 && cp <= 2093) return true;
    if (cp >= 2096 && cp <= 2110) return true;
    if (cp >= 2112 && cp <= 2139) return true;
    if (cp == 2142) return true;
    if (cp >= 2144 && cp <= 2154) return true;
    if (cp >= 2208 && cp <= 2228) return true;
    if (cp >= 2230 && cp <= 2237) return true;
    if (cp >= 2260 && cp <= 2435) return true;
    if (cp >= 2437 && cp <= 2444) return true;
    if (cp >= 2447 && cp <= 2448) return true;
    if (cp >= 2451 && cp <= 2472) return true;
    if (cp >= 2474 && cp <= 2480) return true;
    if (cp == 2482) return true;
    if (cp >= 2486 && cp <= 2489) return true;
    if (cp >= 2492 && cp <= 2500) return true;
    if (cp >= 2503 && cp <= 2504) return true;
    if (cp >= 2507 && cp <= 2510) return true;
    if (cp == 2519) return true;
    if (cp >= 2524 && cp <= 2525) return true;
    if (cp >= 2527 && cp <= 2531) return true;
    if (cp >= 2534 && cp <= 2557) return true;
    if (cp >= 2561 && cp <= 2563) return true;
    if (cp >= 2565 && cp <= 2570) return true;
    if (cp >= 2575 && cp <= 2576) return true;
    if (cp >= 2579 && cp <= 2600) return true;
    if (cp >= 2602 && cp <= 2608) return true;
    if (cp >= 2610 && cp <= 2611) return true;
    if (cp >= 2613 && cp <= 2614) return true;
    if (cp >= 2616 && cp <= 2617) return true;
    if (cp == 2620) return true;
    if (cp >= 2622 && cp <= 2626) return true;
    if (cp >= 2631 && cp <= 2632) return true;
    if (cp >= 2635 && cp <= 2637) return true;
    if (cp == 2641) return true;
    if (cp >= 2649 && cp <= 2652) return true;
    if (cp == 2654) return true;
    if (cp >= 2662 && cp <= 2677) return true;
    if (cp >= 2689 && cp <= 2691) return true;
    if (cp >= 2693 && cp <= 2701) return true;
    if (cp >= 2703 && cp <= 2705) return true;
    if (cp >= 2707 && cp <= 2728) return true;
    if (cp >= 2730 && cp <= 2736) return true;
    if (cp >= 2738 && cp <= 2739) return true;
    if (cp >= 2741 && cp <= 2745) return true;
    if (cp >= 2748 && cp <= 2757) return true;
    if (cp >= 2759 && cp <= 2761) return true;
    if (cp >= 2763 && cp <= 2765) return true;
    if (cp == 2768) return true;
    if (cp >= 2784 && cp <= 2787) return true;
    if (cp >= 2790 && cp <= 2801) return true;
    if (cp >= 2809 && cp <= 2815) return true;
    if (cp >= 2817 && cp <= 2819) return true;
    if (cp >= 2821 && cp <= 2828) return true;
    if (cp >= 2831 && cp <= 2832) return true;
    if (cp >= 2835 && cp <= 2856) return true;
    if (cp >= 2858 && cp <= 2864) return true;
    if (cp >= 2866 && cp <= 2867) return true;
    if (cp >= 2869 && cp <= 2873) return true;
    if (cp >= 2876 && cp <= 2884) return true;
    if (cp >= 2887 && cp <= 2888) return true;
    if (cp >= 2891 && cp <= 2893) return true;
    if (cp >= 2902 && cp <= 2903) return true;
    if (cp >= 2908 && cp <= 2909) return true;
    if (cp >= 2911 && cp <= 2915) return true;
    if (cp >= 2918 && cp <= 2935) return true;
    if (cp >= 2946 && cp <= 2947) return true;
    if (cp >= 2949 && cp <= 2954) return true;
    if (cp >= 2958 && cp <= 2960) return true;
    if (cp >= 2962 && cp <= 2965) return true;
    if (cp >= 2969 && cp <= 2970) return true;
    if (cp == 2972) return true;
    if (cp >= 2974 && cp <= 2975) return true;
    if (cp >= 2979 && cp <= 2980) return true;
    if (cp >= 2984 && cp <= 2986) return true;
    if (cp >= 2990 && cp <= 3001) return true;
    if (cp >= 3006 && cp <= 3010) return true;
    if (cp >= 3014 && cp <= 3016) return true;
    if (cp >= 3018 && cp <= 3021) return true;
    if (cp == 3024) return true;
    if (cp == 3031) return true;
    if (cp >= 3046 && cp <= 3066) return true;
    if (cp >= 3072 && cp <= 3075) return true;
    if (cp >= 3077 && cp <= 3084) return true;
    if (cp >= 3086 && cp <= 3088) return true;
    if (cp >= 3090 && cp <= 3112) return true;
    if (cp >= 3114 && cp <= 3129) return true;
    if (cp >= 3133 && cp <= 3140) return true;
    if (cp >= 3142 && cp <= 3144) return true;
    if (cp >= 3146 && cp <= 3149) return true;
    if (cp >= 3157 && cp <= 3158) return true;
    if (cp >= 3160 && cp <= 3162) return true;
    if (cp >= 3168 && cp <= 3171) return true;
    if (cp >= 3174 && cp <= 3183) return true;
    if (cp >= 3192 && cp <= 3203) return true;
    if (cp >= 3205 && cp <= 3212) return true;
    if (cp >= 3214 && cp <= 3216) return true;
    if (cp >= 3218 && cp <= 3240) return true;
    if (cp >= 3242 && cp <= 3251) return true;
    if (cp >= 3253 && cp <= 3257) return true;
    if (cp >= 3260 && cp <= 3268) return true;
    if (cp >= 3270 && cp <= 3272) return true;
    if (cp >= 3274 && cp <= 3277) return true;
    if (cp >= 3285 && cp <= 3286) return true;
    if (cp == 3294) return true;
    if (cp >= 3296 && cp <= 3299) return true;
    if (cp >= 3302 && cp <= 3311) return true;
    if (cp >= 3313 && cp <= 3314) return true;
    if (cp >= 3328 && cp <= 3331) return true;
    if (cp >= 3333 && cp <= 3340) return true;
    if (cp >= 3342 && cp <= 3344) return true;
    if (cp >= 3346 && cp <= 3396) return true;
    if (cp >= 3398 && cp <= 3400) return true;
    if (cp >= 3402 && cp <= 3407) return true;
    if (cp >= 3412 && cp <= 3427) return true;
    if (cp >= 3430 && cp <= 3455) return true;
    if (cp >= 3458 && cp <= 3459) return true;
    if (cp >= 3461 && cp <= 3478) return true;
    if (cp >= 3482 && cp <= 3505) return true;
    if (cp >= 3507 && cp <= 3515) return true;
    if (cp == 3517) return true;
    if (cp >= 3520 && cp <= 3526) return true;
    if (cp == 3530) return true;
    if (cp >= 3535 && cp <= 3540) return true;
    if (cp == 3542) return true;
    if (cp >= 3544 && cp <= 3551) return true;
    if (cp >= 3558 && cp <= 3567) return true;
    if (cp >= 3570 && cp <= 3572) return true;
    if (cp >= 3585 && cp <= 3642) return true;
    if (cp >= 3647 && cp <= 3675) return true;
    if (cp >= 3713 && cp <= 3714) return true;
    if (cp == 3716) return true;
    if (cp >= 3719 && cp <= 3720) return true;
    if (cp == 3722) return true;
    if (cp == 3725) return true;
    if (cp >= 3732 && cp <= 3735) return true;
    if (cp >= 3737 && cp <= 3743) return true;
    if (cp >= 3745 && cp <= 3747) return true;
    if (cp == 3749) return true;
    if (cp == 3751) return true;
    if (cp >= 3754 && cp <= 3755) return true;
    if (cp >= 3757 && cp <= 3769) return true;
    if (cp >= 3771 && cp <= 3773) return true;
    if (cp >= 3776 && cp <= 3780) return true;
    if (cp == 3782) return true;
    if (cp >= 3784 && cp <= 3789) return true;
    if (cp >= 3792 && cp <= 3801) return true;
    if (cp >= 3804 && cp <= 3807) return true;
    if (cp >= 3840 && cp <= 3911) return true;
    if (cp >= 3913 && cp <= 3948) return true;
    if (cp >= 3953 && cp <= 3991) return true;
    if (cp >= 3993 && cp <= 4028) return true;
    if (cp >= 4030 && cp <= 4044) return true;
    if (cp >= 4046 && cp <= 4058) return true;
    if (cp >= 4096 && cp <= 4293) return true;
    if (cp == 4295) return true;
    if (cp == 4301) return true;
    if (cp >= 4304 && cp <= 4680) return true;
    if (cp >= 4682 && cp <= 4685) return true;
    if (cp >= 4688 && cp <= 4694) return true;
    if (cp == 4696) return true;
    if (cp >= 4698 && cp <= 4701) return true;
    if (cp >= 4704 && cp <= 4744) return true;
    if (cp >= 4746 && cp <= 4749) return true;
    if (cp >= 4752 && cp <= 4784) return true;
    if (cp >= 4786 && cp <= 4789) return true;
    if (cp >= 4792 && cp <= 4798) return true;
    if (cp == 4800) return true;
    if (cp >= 4802 && cp <= 4805) return true;
    if (cp >= 4808 && cp <= 4822) return true;
    if (cp >= 4824 && cp <= 4880) return true;
    if (cp >= 4882 && cp <= 4885) return true;
    if (cp >= 4888 && cp <= 4954) return true;
    if (cp >= 4957 && cp <= 4988) return true;
    if (cp >= 4992 && cp <= 5017) return true;
    if (cp >= 5024 && cp <= 5109) return true;
    if (cp >= 5112 && cp <= 5117) return true;
    if (cp >= 5120 && cp <= 5788) return true;
    if (cp >= 5792 && cp <= 5880) return true;
    if (cp >= 5888 && cp <= 5900) return true;
    if (cp >= 5902 && cp <= 5908) return true;
    if (cp >= 5920 && cp <= 5942) return true;
    if (cp >= 5952 && cp <= 5971) return true;
    if (cp >= 5984 && cp <= 5996) return true;
    if (cp >= 5998 && cp <= 6000) return true;
    if (cp >= 6002 && cp <= 6003) return true;
    if (cp >= 6016 && cp <= 6109) return true;
    if (cp >= 6112 && cp <= 6121) return true;
    if (cp >= 6128 && cp <= 6137) return true;
    if (cp >= 6144 && cp <= 6158) return true;
    if (cp >= 6160 && cp <= 6169) return true;
    if (cp >= 6176 && cp <= 6263) return true;
    if (cp >= 6272 && cp <= 6314) return true;
    if (cp >= 6320 && cp <= 6389) return true;
    if (cp >= 6400 && cp <= 6430) return true;
    if (cp >= 6432 && cp <= 6443) return true;
    if (cp >= 6448 && cp <= 6459) return true;
    if (cp == 6464) return true;
    if (cp >= 6468 && cp <= 6509) return true;
    if (cp >= 6512 && cp <= 6516) return true;
    if (cp >= 6528 && cp <= 6571) return true;
    if (cp >= 6576 && cp <= 6601) return true;
    if (cp >= 6608 && cp <= 6618) return true;
    if (cp >= 6622 && cp <= 6683) return true;
    if (cp >= 6686 && cp <= 6750) return true;
    if (cp >= 6752 && cp <= 6780) return true;
    if (cp >= 6783 && cp <= 6793) return true;
    if (cp >= 6800 && cp <= 6809) return true;
    if (cp >= 6816 && cp <= 6829) return true;
    if (cp >= 6832 && cp <= 6846) return true;
    if (cp >= 6912 && cp <= 6987) return true;
    if (cp >= 6992 && cp <= 7036) return true;
    if (cp >= 7040 && cp <= 7155) return true;
    if (cp >= 7164 && cp <= 7223) return true;
    if (cp >= 7227 && cp <= 7241) return true;
    if (cp >= 7245 && cp <= 7304) return true;
    if (cp >= 7360 && cp <= 7367) return true;
    if (cp >= 7376 && cp <= 7417) return true;
    if (cp >= 7424 && cp <= 7673) return true;
    if (cp >= 7675 && cp <= 7957) return true;
    if (cp >= 7960 && cp <= 7965) return true;
    if (cp >= 7968 && cp <= 8005) return true;
    if (cp >= 8008 && cp <= 8013) return true;
    if (cp >= 8016 && cp <= 8023) return true;
    if (cp == 8025) return true;
    if (cp == 8027) return true;
    if (cp == 8029) return true;
    if (cp >= 8031 && cp <= 8061) return true;
    if (cp >= 8064 && cp <= 8116) return true;
    if (cp >= 8118 && cp <= 8132) return true;
    if (cp >= 8134 && cp <= 8147) return true;
    if (cp >= 8150 && cp <= 8155) return true;
    if (cp >= 8157 && cp <= 8175) return true;
    if (cp >= 8178 && cp <= 8180) return true;
    if (cp >= 8182 && cp <= 8190) return true;
    if (cp >= 8192 && cp <= 8231) return true;
    if (cp >= 8234 && cp <= 8292) return true;
    if (cp >= 8294 && cp <= 8305) return true;
    if (cp >= 8308 && cp <= 8334) return true;
    if (cp >= 8336 && cp <= 8348) return true;
    if (cp >= 8352 && cp <= 8383) return true;
    if (cp >= 8400 && cp <= 8432) return true;
    if (cp >= 8448 && cp <= 8587) return true;
    if (cp >= 8592 && cp <= 9254) return true;
    if (cp >= 9280 && cp <= 9290) return true;
    if (cp >= 9312 && cp <= 11123) return true;
    if (cp >= 11126 && cp <= 11157) return true;
    if (cp >= 11160 && cp <= 11193) return true;
    if (cp >= 11197 && cp <= 11208) return true;
    if (cp >= 11210 && cp <= 11218) return true;
    if (cp >= 11244 && cp <= 11247) return true;
    if (cp >= 11264 && cp <= 11310) return true;
    if (cp >= 11312 && cp <= 11358) return true;
    if (cp >= 11360 && cp <= 11507) return true;
    if (cp >= 11513 && cp <= 11557) return true;
    if (cp == 11559) return true;
    if (cp == 11565) return true;
    if (cp >= 11568 && cp <= 11623) return true;
    if (cp >= 11631 && cp <= 11632) return true;
    if (cp >= 11647 && cp <= 11670) return true;
    if (cp >= 11680 && cp <= 11686) return true;
    if (cp >= 11688 && cp <= 11694) return true;
    if (cp >= 11696 && cp <= 11702) return true;
    if (cp >= 11704 && cp <= 11710) return true;
    if (cp >= 11712 && cp <= 11718) return true;
    if (cp >= 11720 && cp <= 11726) return true;
    if (cp >= 11728 && cp <= 11734) return true;
    if (cp >= 11736 && cp <= 11742) return true;
    if (cp >= 11744 && cp <= 11849) return true;
    if (cp >= 11904 && cp <= 11929) return true;
    if (cp >= 11931 && cp <= 12019) return true;
    if (cp >= 12032 && cp <= 12245) return true;
    if (cp >= 12272 && cp <= 12283) return true;
    if (cp >= 12288 && cp <= 12351) return true;
    if (cp >= 12353 && cp <= 12438) return true;
    if (cp >= 12441 && cp <= 12543) return true;
    if (cp >= 12549 && cp <= 12590) return true;
    if (cp >= 12593 && cp <= 12686) return true;
    if (cp >= 12688 && cp <= 12730) return true;
    if (cp >= 12736 && cp <= 12771) return true;
    if (cp >= 12784 && cp <= 12830) return true;
    if (cp >= 12832 && cp <= 13054) return true;
    if (cp >= 13056 && cp <= 19893) return true;
    if (cp >= 19904 && cp <= 40938) return true;
    if (cp >= 40960 && cp <= 42124) return true;
    if (cp >= 42128 && cp <= 42182) return true;
    if (cp >= 42192 && cp <= 42539) return true;
    if (cp >= 42560 && cp <= 42743) return true;
    if (cp >= 42752 && cp <= 42926) return true;
    if (cp >= 42928 && cp <= 42935) return true;
    if (cp >= 42999 && cp <= 43051) return true;
    if (cp >= 43056 && cp <= 43065) return true;
    if (cp >= 43072 && cp <= 43127) return true;
    if (cp >= 43136 && cp <= 43205) return true;
    if (cp >= 43214 && cp <= 43225) return true;
    if (cp >= 43232 && cp <= 43261) return true;
    if (cp >= 43264 && cp <= 43347) return true;
    if (cp >= 43359 && cp <= 43388) return true;
    if (cp >= 43392 && cp <= 43469) return true;
    if (cp >= 43471 && cp <= 43481) return true;
    if (cp >= 43486 && cp <= 43518) return true;
    if (cp >= 43520 && cp <= 43574) return true;
    if (cp >= 43584 && cp <= 43597) return true;
    if (cp >= 43600 && cp <= 43609) return true;
    if (cp >= 43612 && cp <= 43714) return true;
    if (cp >= 43739 && cp <= 43766) return true;
    if (cp >= 43777 && cp <= 43782) return true;
    if (cp >= 43785 && cp <= 43790) return true;
    if (cp >= 43793 && cp <= 43798) return true;
    if (cp >= 43808 && cp <= 43814) return true;
    if (cp >= 43816 && cp <= 43822) return true;
    if (cp >= 43824 && cp <= 43877) return true;
    if (cp >= 43888 && cp <= 44013) return true;
    if (cp >= 44016 && cp <= 44025) return true;
    if (cp >= 44032 && cp <= 55203) return true;
    if (cp >= 55216 && cp <= 55238) return true;
    if (cp >= 55243 && cp <= 55291) return true;
    if (cp >= 57344 && cp <= 64109) return true;
    if (cp >= 64112 && cp <= 64217) return true;
    if (cp >= 64256 && cp <= 64262) return true;
    if (cp >= 64275 && cp <= 64279) return true;
    if (cp >= 64285 && cp <= 64310) return true;
    if (cp >= 64312 && cp <= 64316) return true;
    if (cp == 64318) return true;
    if (cp >= 64320 && cp <= 64321) return true;
    if (cp >= 64323 && cp <= 64324) return true;
    if (cp >= 64326 && cp <= 64449) return true;
    if (cp >= 64467 && cp <= 64831) return true;
    if (cp >= 64848 && cp <= 64911) return true;
    if (cp >= 64914 && cp <= 64967) return true;
    if (cp >= 65008 && cp <= 65021) return true;
    if (cp >= 65024 && cp <= 65049) return true;
    if (cp >= 65056 && cp <= 65106) return true;
    if (cp >= 65108 && cp <= 65126) return true;
    if (cp >= 65128 && cp <= 65131) return true;
    if (cp >= 65136 && cp <= 65140) return true;
    if (cp >= 65142 && cp <= 65276) return true;
    if (cp == 65279) return true;
    if (cp >= 65281 && cp <= 65470) return true;
    if (cp >= 65474 && cp <= 65479) return true;
    if (cp >= 65482 && cp <= 65487) return true;
    if (cp >= 65490 && cp <= 65495) return true;
    if (cp >= 65498 && cp <= 65500) return true;
    if (cp >= 65504 && cp <= 65510) return true;
    if (cp >= 65512 && cp <= 65518) return true;
    if (cp >= 65529 && cp <= 65533) return true;
    if (cp >= 65536 && cp <= 65547) return true;
    if (cp >= 65549 && cp <= 65574) return true;
    if (cp >= 65576 && cp <= 65594) return true;
    if (cp >= 65596 && cp <= 65597) return true;
    if (cp >= 65599 && cp <= 65613) return true;
    if (cp >= 65616 && cp <= 65629) return true;
    if (cp >= 65664 && cp <= 65786) return true;
    if (cp >= 65792 && cp <= 65794) return true;
    if (cp >= 65799 && cp <= 65843) return true;
    if (cp >= 65847 && cp <= 65934) return true;
    if (cp >= 65936 && cp <= 65947) return true;
    if (cp == 65952) return true;
    if (cp >= 66000 && cp <= 66045) return true;
    if (cp >= 66176 && cp <= 66204) return true;
    if (cp >= 66208 && cp <= 66256) return true;
    if (cp >= 66272 && cp <= 66299) return true;
    if (cp >= 66304 && cp <= 66339) return true;
    if (cp >= 66349 && cp <= 66378) return true;
    if (cp >= 66384 && cp <= 66426) return true;
    if (cp >= 66432 && cp <= 66461) return true;
    if (cp >= 66463 && cp <= 66499) return true;
    if (cp >= 66504 && cp <= 66517) return true;
    if (cp >= 66560 && cp <= 66717) return true;
    if (cp >= 66720 && cp <= 66729) return true;
    if (cp >= 66736 && cp <= 66771) return true;
    if (cp >= 66776 && cp <= 66811) return true;
    if (cp >= 66816 && cp <= 66855) return true;
    if (cp >= 66864 && cp <= 66915) return true;
    if (cp == 66927) return true;
    if (cp >= 67072 && cp <= 67382) return true;
    if (cp >= 67392 && cp <= 67413) return true;
    if (cp >= 67424 && cp <= 67431) return true;
    if (cp >= 67584 && cp <= 67589) return true;
    if (cp == 67592) return true;
    if (cp >= 67594 && cp <= 67637) return true;
    if (cp >= 67639 && cp <= 67640) return true;
    if (cp == 67644) return true;
    if (cp >= 67647 && cp <= 67669) return true;
    if (cp >= 67671 && cp <= 67742) return true;
    if (cp >= 67751 && cp <= 67759) return true;
    if (cp >= 67808 && cp <= 67826) return true;
    if (cp >= 67828 && cp <= 67829) return true;
    if (cp >= 67835 && cp <= 67867) return true;
    if (cp >= 67871 && cp <= 67897) return true;
    if (cp == 67903) return true;
    if (cp >= 67968 && cp <= 68023) return true;
    if (cp >= 68028 && cp <= 68047) return true;
    if (cp >= 68050 && cp <= 68099) return true;
    if (cp >= 68101 && cp <= 68102) return true;
    if (cp >= 68108 && cp <= 68115) return true;
    if (cp >= 68117 && cp <= 68119) return true;
    if (cp >= 68121 && cp <= 68147) return true;
    if (cp >= 68152 && cp <= 68154) return true;
    if (cp >= 68159 && cp <= 68167) return true;
    if (cp >= 68176 && cp <= 68184) return true;
    if (cp >= 68192 && cp <= 68255) return true;
    if (cp >= 68288 && cp <= 68326) return true;
    if (cp >= 68331 && cp <= 68342) return true;
    if (cp >= 68352 && cp <= 68405) return true;
    if (cp >= 68409 && cp <= 68437) return true;
    if (cp >= 68440 && cp <= 68466) return true;
    if (cp >= 68472 && cp <= 68497) return true;
    if (cp >= 68505 && cp <= 68508) return true;
    if (cp >= 68521 && cp <= 68527) return true;
    if (cp >= 68608 && cp <= 68680) return true;
    if (cp >= 68736 && cp <= 68786) return true;
    if (cp >= 68800 && cp <= 68850) return true;
    if (cp >= 68858 && cp <= 68863) return true;
    if (cp >= 69216 && cp <= 69246) return true;
    if (cp >= 69632 && cp <= 69709) return true;
    if (cp >= 69714 && cp <= 69743) return true;
    if (cp >= 69759 && cp <= 69825) return true;
    if (cp >= 69840 && cp <= 69864) return true;
    if (cp >= 69872 && cp <= 69881) return true;
    if (cp >= 69888 && cp <= 69940) return true;
    if (cp >= 69942 && cp <= 69955) return true;
    if (cp >= 69968 && cp <= 70006) return true;
    if (cp >= 70016 && cp <= 70093) return true;
    if (cp >= 70096 && cp <= 70111) return true;
    if (cp >= 70113 && cp <= 70132) return true;
    if (cp >= 70144 && cp <= 70161) return true;
    if (cp >= 70163 && cp <= 70206) return true;
    if (cp >= 70272 && cp <= 70278) return true;
    if (cp == 70280) return true;
    if (cp >= 70282 && cp <= 70285) return true;
    if (cp >= 70287 && cp <= 70301) return true;
    if (cp >= 70303 && cp <= 70313) return true;
    if (cp >= 70320 && cp <= 70378) return true;
    if (cp >= 70384 && cp <= 70393) return true;
    if (cp >= 70400 && cp <= 70403) return true;
    if (cp >= 70405 && cp <= 70412) return true;
    if (cp >= 70415 && cp <= 70416) return true;
    if (cp >= 70419 && cp <= 70440) return true;
    if (cp >= 70442 && cp <= 70448) return true;
    if (cp >= 70450 && cp <= 70451) return true;
    if (cp >= 70453 && cp <= 70457) return true;
    if (cp >= 70460 && cp <= 70468) return true;
    if (cp >= 70471 && cp <= 70472) return true;
    if (cp >= 70475 && cp <= 70477) return true;
    if (cp == 70480) return true;
    if (cp == 70487) return true;
    if (cp >= 70493 && cp <= 70499) return true;
    if (cp >= 70502 && cp <= 70508) return true;
    if (cp >= 70512 && cp <= 70516) return true;
    if (cp >= 70656 && cp <= 70745) return true;
    if (cp == 70747) return true;
    if (cp == 70749) return true;
    if (cp >= 70784 && cp <= 70855) return true;
    if (cp >= 70864 && cp <= 70873) return true;
    if (cp >= 71040 && cp <= 71093) return true;
    if (cp >= 71096 && cp <= 71133) return true;
    if (cp >= 71168 && cp <= 71236) return true;
    if (cp >= 71248 && cp <= 71257) return true;
    if (cp >= 71264 && cp <= 71276) return true;
    if (cp >= 71296 && cp <= 71351) return true;
    if (cp >= 71360 && cp <= 71369) return true;
    if (cp >= 71424 && cp <= 71449) return true;
    if (cp >= 71453 && cp <= 71467) return true;
    if (cp >= 71472 && cp <= 71487) return true;
    if (cp >= 71840 && cp <= 71922) return true;
    if (cp == 71935) return true;
    if (cp >= 72192 && cp <= 72263) return true;
    if (cp >= 72272 && cp <= 72323) return true;
    if (cp >= 72326 && cp <= 72348) return true;
    if (cp >= 72350 && cp <= 72354) return true;
    if (cp >= 72384 && cp <= 72440) return true;
    if (cp >= 72704 && cp <= 72712) return true;
    if (cp >= 72714 && cp <= 72758) return true;
    if (cp >= 72760 && cp <= 72773) return true;
    if (cp >= 72784 && cp <= 72812) return true;
    if (cp >= 72816 && cp <= 72847) return true;
    if (cp >= 72850 && cp <= 72871) return true;
    if (cp >= 72873 && cp <= 72886) return true;
    if (cp >= 72960 && cp <= 72966) return true;
    if (cp >= 72968 && cp <= 72969) return true;
    if (cp >= 72971 && cp <= 73014) return true;
    if (cp == 73018) return true;
    if (cp >= 73020 && cp <= 73021) return true;
    if (cp >= 73023 && cp <= 73031) return true;
    if (cp >= 73040 && cp <= 73049) return true;
    if (cp >= 73728 && cp <= 74649) return true;
    if (cp >= 74752 && cp <= 74862) return true;
    if (cp >= 74864 && cp <= 74868) return true;
    if (cp >= 74880 && cp <= 75075) return true;
    if (cp >= 77824 && cp <= 78894) return true;
    if (cp >= 82944 && cp <= 83526) return true;
    if (cp >= 92160 && cp <= 92728) return true;
    if (cp >= 92736 && cp <= 92766) return true;
    if (cp >= 92768 && cp <= 92777) return true;
    if (cp >= 92782 && cp <= 92783) return true;
    if (cp >= 92880 && cp <= 92909) return true;
    if (cp >= 92912 && cp <= 92917) return true;
    if (cp >= 92928 && cp <= 92997) return true;
    if (cp >= 93008 && cp <= 93017) return true;
    if (cp >= 93019 && cp <= 93025) return true;
    if (cp >= 93027 && cp <= 93047) return true;
    if (cp >= 93053 && cp <= 93071) return true;
    if (cp >= 93952 && cp <= 94020) return true;
    if (cp >= 94032 && cp <= 94078) return true;
    if (cp >= 94095 && cp <= 94111) return true;
    if (cp >= 94176 && cp <= 94177) return true;
    if (cp >= 94208 && cp <= 100332) return true;
    if (cp >= 100352 && cp <= 101106) return true;
    if (cp >= 110592 && cp <= 110878) return true;
    if (cp >= 110960 && cp <= 111355) return true;
    if (cp >= 113664 && cp <= 113770) return true;
    if (cp >= 113776 && cp <= 113788) return true;
    if (cp >= 113792 && cp <= 113800) return true;
    if (cp >= 113808 && cp <= 113817) return true;
    if (cp >= 113820 && cp <= 113827) return true;
    if (cp >= 118784 && cp <= 119029) return true;
    if (cp >= 119040 && cp <= 119078) return true;
    if (cp >= 119081 && cp <= 119272) return true;
    if (cp >= 119296 && cp <= 119365) return true;
    if (cp >= 119552 && cp <= 119638) return true;
    if (cp >= 119648 && cp <= 119665) return true;
    if (cp >= 119808 && cp <= 119892) return true;
    if (cp >= 119894 && cp <= 119964) return true;
    if (cp >= 119966 && cp <= 119967) return true;
    if (cp == 119970) return true;
    if (cp >= 119973 && cp <= 119974) return true;
    if (cp >= 119977 && cp <= 119980) return true;
    if (cp >= 119982 && cp <= 119993) return true;
    if (cp == 119995) return true;
    if (cp >= 119997 && cp <= 120003) return true;
    if (cp >= 120005 && cp <= 120069) return true;
    if (cp >= 120071 && cp <= 120074) return true;
    if (cp >= 120077 && cp <= 120084) return true;
    if (cp >= 120086 && cp <= 120092) return true;
    if (cp >= 120094 && cp <= 120121) return true;
    if (cp >= 120123 && cp <= 120126) return true;
    if (cp >= 120128 && cp <= 120132) return true;
    if (cp == 120134) return true;
    if (cp >= 120138 && cp <= 120144) return true;
    if (cp >= 120146 && cp <= 120485) return true;
    if (cp >= 120488 && cp <= 120779) return true;
    if (cp >= 120782 && cp <= 121483) return true;
    if (cp >= 121499 && cp <= 121503) return true;
    if (cp >= 121505 && cp <= 121519) return true;
    if (cp >= 122880 && cp <= 122886) return true;
    if (cp >= 122888 && cp <= 122904) return true;
    if (cp >= 122907 && cp <= 122913) return true;
    if (cp >= 122915 && cp <= 122916) return true;
    if (cp >= 122918 && cp <= 122922) return true;
    if (cp >= 124928 && cp <= 125124) return true;
    if (cp >= 125127 && cp <= 125142) return true;
    if (cp >= 125184 && cp <= 125258) return true;
    if (cp >= 125264 && cp <= 125273) return true;
    if (cp >= 125278 && cp <= 125279) return true;
    if (cp >= 126464 && cp <= 126467) return true;
    if (cp >= 126469 && cp <= 126495) return true;
    if (cp >= 126497 && cp <= 126498) return true;
    if (cp == 126500) return true;
    if (cp == 126503) return true;
    if (cp >= 126505 && cp <= 126514) return true;
    if (cp >= 126516 && cp <= 126519) return true;
    if (cp == 126521) return true;
    if (cp == 126523) return true;
    if (cp == 126530) return true;
    if (cp == 126535) return true;
    if (cp == 126537) return true;
    if (cp == 126539) return true;
    if (cp >= 126541 && cp <= 126543) return true;
    if (cp >= 126545 && cp <= 126546) return true;
    if (cp == 126548) return true;
    if (cp == 126551) return true;
    if (cp == 126553) return true;
    if (cp == 126555) return true;
    if (cp == 126557) return true;
    if (cp == 126559) return true;
    if (cp >= 126561 && cp <= 126562) return true;
    if (cp == 126564) return true;
    if (cp >= 126567 && cp <= 126570) return true;
    if (cp >= 126572 && cp <= 126578) return true;
    if (cp >= 126580 && cp <= 126583) return true;
    if (cp >= 126585 && cp <= 126588) return true;
    if (cp == 126590) return true;
    if (cp >= 126592 && cp <= 126601) return true;
    if (cp >= 126603 && cp <= 126619) return true;
    if (cp >= 126625 && cp <= 126627) return true;
    if (cp >= 126629 && cp <= 126633) return true;
    if (cp >= 126635 && cp <= 126651) return true;
    if (cp >= 126704 && cp <= 126705) return true;
    if (cp >= 126976 && cp <= 127019) return true;
    if (cp >= 127024 && cp <= 127123) return true;
    if (cp >= 127136 && cp <= 127150) return true;
    if (cp >= 127153 && cp <= 127167) return true;
    if (cp >= 127169 && cp <= 127183) return true;
    if (cp >= 127185 && cp <= 127221) return true;
    if (cp >= 127232 && cp <= 127244) return true;
    if (cp >= 127248 && cp <= 127278) return true;
    if (cp >= 127280 && cp <= 127339) return true;
    if (cp >= 127344 && cp <= 127404) return true;
    if (cp >= 127462 && cp <= 127490) return true;
    if (cp >= 127504 && cp <= 127547) return true;
    if (cp >= 127552 && cp <= 127560) return true;
    if (cp >= 127568 && cp <= 127569) return true;
    if (cp >= 127584 && cp <= 127589) return true;
    if (cp >= 127744 && cp <= 128724) return true;
    if (cp >= 128736 && cp <= 128748) return true;
    if (cp >= 128752 && cp <= 128760) return true;
    if (cp >= 128768 && cp <= 128883) return true;
    if (cp >= 128896 && cp <= 128980) return true;
    if (cp >= 129024 && cp <= 129035) return true;
    if (cp >= 129040 && cp <= 129095) return true;
    if (cp >= 129104 && cp <= 129113) return true;
    if (cp >= 129120 && cp <= 129159) return true;
    if (cp >= 129168 && cp <= 129197) return true;
    if (cp >= 129280 && cp <= 129291) return true;
    if (cp >= 129296 && cp <= 129342) return true;
    if (cp >= 129344 && cp <= 129356) return true;
    if (cp >= 129360 && cp <= 129387) return true;
    if (cp >= 129408 && cp <= 129431) return true;
    if (cp == 129472) return true;
    if (cp >= 129488 && cp <= 129510) return true;
    if (cp >= 131072 && cp <= 173782) return true;
    if (cp >= 173824 && cp <= 177972) return true;
    if (cp >= 177984 && cp <= 178205) return true;
    if (cp >= 178208 && cp <= 183969) return true;
    if (cp >= 183984 && cp <= 191456) return true;
    if (cp >= 194560 && cp <= 195101) return true;
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

function whine(msg) {
    alert(msg);
    throw msg;
}

function pf(loc) {
    throw ("Pattern match failure (" + loc + ")");
}

var lameDuck = false;

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

  if (v != "")
      er("Setting <select> to nonexistent value: " + v);
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
function sub(s, i) { return Array.from(s)[i]; }
function suf(s, i) { return Array.from(s).slice(i).join(""); }
function slen(s) { return Array.from(s).length; }
function sidx(s, ch) {
    var r = Array.from(s).indexOf(ch);
    if (r == -1)
        return null;
    else
        return r;
}
function ssidx(h, n) {
    if (n == "") return 0;
    var ah = Array.from(h);
    var an = Array.from(n);
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
    var s2 = Array.from(s);
    var chs2 = Array.from(chs);
    
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
    return Array.from(s).slice(start, start+len).join("");
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

function requestUri(xhr, uri, needsSig, isRpc) {
    var extraData = null;

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
        if (sig == null)
            whine("Missing cookie signature!");

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
    var chars = Array.from(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != '-' && c != '_'))
            er("Disallowed character in data-* attribute name");
    }

    return s;
}


// CSS validation

function atom(s) {
    var chars = Array.from(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != '+' && c != '-' && c != '.' && c != '%' && c != '#'))
            er("Disallowed character in CSS atom");
    }

    return s;
}

function css_url(s) {
    var chars = Array.from(s);
    
    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isAlnum(c) && c != ':' && c != '/' && c != '.' && c != '_' && c != '+'
			  && c != '-' && c != '%' && c != '?' && c != '&' && c != '=' && c != '#'))
            er("Disallowed character in CSS URL");
    }

    return s;
}

function property(s) {
    var chars = Array.from(s);
    
    if (chars.length <= 0)
        er("Empty CSS property");

    if (chars[0] > maxCh || (!isLower(chars[0]) && chars[0] != '_'))
        er("Bad initial character in CSS property");

    for (var i = 0; i < chars.length; ++i) {
        var c = chars[i];
        if (c > maxCh || (!isLower(c) && !isDigit(c) && c != '_' && c != '-'))
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


// App-specific code
