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

var isLowerHTable = {
	97: true, 98: true, 99: true, 100: true, 101: true, 102: true, 103: true, 104: true, 105: true, 106: true, 107: true, 108: true, 109: true, 110: true, 111: true, 112: true, 113: true, 114: true, 115: true, 116: true, 
	117: true, 118: true, 119: true, 120: true, 121: true, 122: true, 170: true, 181: true, 186: true, 223: true, 224: true, 225: true, 226: true, 227: true, 228: true, 229: true, 230: true, 231: true, 232: true, 233: true, 
	234: true, 235: true, 236: true, 237: true, 238: true, 239: true, 240: true, 241: true, 242: true, 243: true, 244: true, 245: true, 246: true, 248: true, 249: true, 250: true, 251: true, 252: true, 253: true, 254: true, 
	255: true, 257: true, 259: true, 261: true, 263: true, 265: true, 267: true, 269: true, 271: true, 273: true, 275: true, 277: true, 279: true, 281: true, 283: true, 285: true, 287: true, 289: true, 291: true, 293: true, 
	295: true, 297: true, 299: true, 301: true, 303: true, 305: true, 307: true, 309: true, 311: true, 312: true, 314: true, 316: true, 318: true, 320: true, 322: true, 324: true, 326: true, 328: true, 329: true, 331: true, 
	333: true, 335: true, 337: true, 339: true, 341: true, 343: true, 345: true, 347: true, 349: true, 351: true, 353: true, 355: true, 357: true, 359: true, 361: true, 363: true, 365: true, 367: true, 369: true, 371: true, 
	373: true, 375: true, 378: true, 380: true, 382: true, 383: true, 384: true, 387: true, 389: true, 392: true, 396: true, 397: true, 402: true, 405: true, 409: true, 410: true, 411: true, 414: true, 417: true, 419: true, 
	421: true, 424: true, 426: true, 427: true, 429: true, 432: true, 436: true, 438: true, 441: true, 442: true, 445: true, 446: true, 447: true, 454: true, 457: true, 460: true, 462: true, 464: true, 466: true, 468: true, 
	470: true, 472: true, 474: true, 476: true, 477: true, 479: true, 481: true, 483: true, 485: true, 487: true, 489: true, 491: true, 493: true, 495: true, 496: true, 499: true, 501: true, 505: true, 507: true, 509: true, 
	511: true, 513: true, 515: true, 517: true, 519: true, 521: true, 523: true, 525: true, 527: true, 529: true, 531: true, 533: true, 535: true, 537: true, 539: true, 541: true, 543: true, 545: true, 547: true, 549: true, 
	551: true, 553: true, 555: true, 557: true, 559: true, 561: true, 563: true, 564: true, 565: true, 566: true, 567: true, 568: true, 569: true, 572: true, 575: true, 576: true, 578: true, 583: true, 585: true, 587: true, 
	589: true, 591: true, 592: true, 593: true, 594: true, 595: true, 596: true, 597: true, 598: true, 599: true, 600: true, 601: true, 602: true, 603: true, 604: true, 605: true, 606: true, 607: true, 608: true, 609: true, 
	610: true, 611: true, 612: true, 613: true, 614: true, 615: true, 616: true, 617: true, 618: true, 619: true, 620: true, 621: true, 622: true, 623: true, 624: true, 625: true, 626: true, 627: true, 628: true, 629: true, 
	630: true, 631: true, 632: true, 633: true, 634: true, 635: true, 636: true, 637: true, 638: true, 639: true, 640: true, 641: true, 642: true, 643: true, 644: true, 645: true, 646: true, 647: true, 648: true, 649: true, 
	650: true, 651: true, 652: true, 653: true, 654: true, 655: true, 656: true, 657: true, 658: true, 659: true, 661: true, 662: true, 663: true, 664: true, 665: true, 666: true, 667: true, 668: true, 669: true, 670: true, 
	671: true, 672: true, 673: true, 674: true, 675: true, 676: true, 677: true, 678: true, 679: true, 680: true, 681: true, 682: true, 683: true, 684: true, 685: true, 686: true, 687: true, 688: true, 689: true, 690: true, 
	691: true, 692: true, 693: true, 694: true, 695: true, 696: true, 704: true, 705: true, 736: true, 737: true, 738: true, 739: true, 740: true, 837: true, 881: true, 883: true, 887: true, 890: true, 891: true, 892: true, 
	893: true, 912: true, 940: true, 941: true, 942: true, 943: true, 944: true, 945: true, 946: true, 947: true, 948: true, 949: true, 950: true, 951: true, 952: true, 953: true, 954: true, 955: true, 956: true, 957: true, 
	958: true, 959: true, 960: true, 961: true, 962: true, 963: true, 964: true, 965: true, 966: true, 967: true, 968: true, 969: true, 970: true, 971: true, 972: true, 973: true, 974: true, 976: true, 977: true, 981: true, 
	982: true, 983: true, 985: true, 987: true, 989: true, 991: true, 993: true, 995: true, 997: true, 999: true, 1001: true, 1003: true, 1005: true, 1007: true, 1008: true, 1009: true, 1010: true, 1011: true, 1013: true, 1016: true, 
	1019: true, 1020: true, 1072: true, 1073: true, 1074: true, 1075: true, 1076: true, 1077: true, 1078: true, 1079: true, 1080: true, 1081: true, 1082: true, 1083: true, 1084: true, 1085: true, 1086: true, 1087: true, 1088: true, 1089: true, 
	1090: true, 1091: true, 1092: true, 1093: true, 1094: true, 1095: true, 1096: true, 1097: true, 1098: true, 1099: true, 1100: true, 1101: true, 1102: true, 1103: true, 1104: true, 1105: true, 1106: true, 1107: true, 1108: true, 1109: true, 
	1110: true, 1111: true, 1112: true, 1113: true, 1114: true, 1115: true, 1116: true, 1117: true, 1118: true, 1119: true, 1121: true, 1123: true, 1125: true, 1127: true, 1129: true, 1131: true, 1133: true, 1135: true, 1137: true, 1139: true, 
	1141: true, 1143: true, 1145: true, 1147: true, 1149: true, 1151: true, 1153: true, 1163: true, 1165: true, 1167: true, 1169: true, 1171: true, 1173: true, 1175: true, 1177: true, 1179: true, 1181: true, 1183: true, 1185: true, 1187: true, 
	1189: true, 1191: true, 1193: true, 1195: true, 1197: true, 1199: true, 1201: true, 1203: true, 1205: true, 1207: true, 1209: true, 1211: true, 1213: true, 1215: true, 1218: true, 1220: true, 1222: true, 1224: true, 1226: true, 1228: true, 
	1230: true, 1231: true, 1233: true, 1235: true, 1237: true, 1239: true, 1241: true, 1243: true, 1245: true, 1247: true, 1249: true, 1251: true, 1253: true, 1255: true, 1257: true, 1259: true, 1261: true, 1263: true, 1265: true, 1267: true, 
	1269: true, 1271: true, 1273: true, 1275: true, 1277: true, 1279: true, 1281: true, 1283: true, 1285: true, 1287: true, 1289: true, 1291: true, 1293: true, 1295: true, 1297: true, 1299: true, 1301: true, 1303: true, 1305: true, 1307: true, 
	1309: true, 1311: true, 1313: true, 1315: true, 1317: true, 1319: true, 1321: true, 1323: true, 1325: true, 1327: true, 1377: true, 1378: true, 1379: true, 1380: true, 1381: true, 1382: true, 1383: true, 1384: true, 1385: true, 1386: true, 
	1387: true, 1388: true, 1389: true, 1390: true, 1391: true, 1392: true, 1393: true, 1394: true, 1395: true, 1396: true, 1397: true, 1398: true, 1399: true, 1400: true, 1401: true, 1402: true, 1403: true, 1404: true, 1405: true, 1406: true, 
	1407: true, 1408: true, 1409: true, 1410: true, 1411: true, 1412: true, 1413: true, 1414: true, 1415: true, 5112: true, 5113: true, 5114: true, 5115: true, 5116: true, 5117: true, 7296: true, 7297: true, 7298: true, 7299: true, 7300: true, 
	7301: true, 7302: true, 7303: true, 7304: true, 7424: true, 7425: true, 7426: true, 7427: true, 7428: true, 7429: true, 7430: true, 7431: true, 7432: true, 7433: true, 7434: true, 7435: true, 7436: true, 7437: true, 7438: true, 7439: true, 
	7440: true, 7441: true, 7442: true, 7443: true, 7444: true, 7445: true, 7446: true, 7447: true, 7448: true, 7449: true, 7450: true, 7451: true, 7452: true, 7453: true, 7454: true, 7455: true, 7456: true, 7457: true, 7458: true, 7459: true, 
	7460: true, 7461: true, 7462: true, 7463: true, 7464: true, 7465: true, 7466: true, 7467: true, 7468: true, 7469: true, 7470: true, 7471: true, 7472: true, 7473: true, 7474: true, 7475: true, 7476: true, 7477: true, 7478: true, 7479: true, 
	7480: true, 7481: true, 7482: true, 7483: true, 7484: true, 7485: true, 7486: true, 7487: true, 7488: true, 7489: true, 7490: true, 7491: true, 7492: true, 7493: true, 7494: true, 7495: true, 7496: true, 7497: true, 7498: true, 7499: true, 
	7500: true, 7501: true, 7502: true, 7503: true, 7504: true, 7505: true, 7506: true, 7507: true, 7508: true, 7509: true, 7510: true, 7511: true, 7512: true, 7513: true, 7514: true, 7515: true, 7516: true, 7517: true, 7518: true, 7519: true, 
	7520: true, 7521: true, 7522: true, 7523: true, 7524: true, 7525: true, 7526: true, 7527: true, 7528: true, 7529: true, 7530: true, 7531: true, 7532: true, 7533: true, 7534: true, 7535: true, 7536: true, 7537: true, 7538: true, 7539: true, 
	7540: true, 7541: true, 7542: true, 7543: true, 7544: true, 7545: true, 7546: true, 7547: true, 7548: true, 7549: true, 7550: true, 7551: true, 7552: true, 7553: true, 7554: true, 7555: true, 7556: true, 7557: true, 7558: true, 7559: true, 
	7560: true, 7561: true, 7562: true, 7563: true, 7564: true, 7565: true, 7566: true, 7567: true, 7568: true, 7569: true, 7570: true, 7571: true, 7572: true, 7573: true, 7574: true, 7575: true, 7576: true, 7577: true, 7578: true, 7579: true, 
	7580: true, 7581: true, 7582: true, 7583: true, 7584: true, 7585: true, 7586: true, 7587: true, 7588: true, 7589: true, 7590: true, 7591: true, 7592: true, 7593: true, 7594: true, 7595: true, 7596: true, 7597: true, 7598: true, 7599: true, 
	7600: true, 7601: true, 7602: true, 7603: true, 7604: true, 7605: true, 7606: true, 7607: true, 7608: true, 7609: true, 7610: true, 7611: true, 7612: true, 7613: true, 7614: true, 7615: true, 7681: true, 7683: true, 7685: true, 7687: true, 
	7689: true, 7691: true, 7693: true, 7695: true, 7697: true, 7699: true, 7701: true, 7703: true, 7705: true, 7707: true, 7709: true, 7711: true, 7713: true, 7715: true, 7717: true, 7719: true, 7721: true, 7723: true, 7725: true, 7727: true, 
	7729: true, 7731: true, 7733: true, 7735: true, 7737: true, 7739: true, 7741: true, 7743: true, 7745: true, 7747: true, 7749: true, 7751: true, 7753: true, 7755: true, 7757: true, 7759: true, 7761: true, 7763: true, 7765: true, 7767: true, 
	7769: true, 7771: true, 7773: true, 7775: true, 7777: true, 7779: true, 7781: true, 7783: true, 7785: true, 7787: true, 7789: true, 7791: true, 7793: true, 7795: true, 7797: true, 7799: true, 7801: true, 7803: true, 7805: true, 7807: true, 
	7809: true, 7811: true, 7813: true, 7815: true, 7817: true, 7819: true, 7821: true, 7823: true, 7825: true, 7827: true, 7829: true, 7830: true, 7831: true, 7832: true, 7833: true, 7834: true, 7835: true, 7836: true, 7837: true, 7839: true, 
	7841: true, 7843: true, 7845: true, 7847: true, 7849: true, 7851: true, 7853: true, 7855: true, 7857: true, 7859: true, 7861: true, 7863: true, 7865: true, 7867: true, 7869: true, 7871: true, 7873: true, 7875: true, 7877: true, 7879: true, 
	7881: true, 7883: true, 7885: true, 7887: true, 7889: true, 7891: true, 7893: true, 7895: true, 7897: true, 7899: true, 7901: true, 7903: true, 7905: true, 7907: true, 7909: true, 7911: true, 7913: true, 7915: true, 7917: true, 7919: true, 
	7921: true, 7923: true, 7925: true, 7927: true, 7929: true, 7931: true, 7933: true, 7935: true, 7936: true, 7937: true, 7938: true, 7939: true, 7940: true, 7941: true, 7942: true, 7943: true, 7952: true, 7953: true, 7954: true, 7955: true, 
	7956: true, 7957: true, 7968: true, 7969: true, 7970: true, 7971: true, 7972: true, 7973: true, 7974: true, 7975: true, 7984: true, 7985: true, 7986: true, 7987: true, 7988: true, 7989: true, 7990: true, 7991: true, 8000: true, 8001: true, 
	8002: true, 8003: true, 8004: true, 8005: true, 8016: true, 8017: true, 8018: true, 8019: true, 8020: true, 8021: true, 8022: true, 8023: true, 8032: true, 8033: true, 8034: true, 8035: true, 8036: true, 8037: true, 8038: true, 8039: true, 
	8048: true, 8049: true, 8050: true, 8051: true, 8052: true, 8053: true, 8054: true, 8055: true, 8056: true, 8057: true, 8058: true, 8059: true, 8060: true, 8061: true, 8064: true, 8065: true, 8066: true, 8067: true, 8068: true, 8069: true, 
	8070: true, 8071: true, 8080: true, 8081: true, 8082: true, 8083: true, 8084: true, 8085: true, 8086: true, 8087: true, 8096: true, 8097: true, 8098: true, 8099: true, 8100: true, 8101: true, 8102: true, 8103: true, 8112: true, 8113: true, 
	8114: true, 8115: true, 8116: true, 8118: true, 8119: true, 8126: true, 8130: true, 8131: true, 8132: true, 8134: true, 8135: true, 8144: true, 8145: true, 8146: true, 8147: true, 8150: true, 8151: true, 8160: true, 8161: true, 8162: true, 
	8163: true, 8164: true, 8165: true, 8166: true, 8167: true, 8178: true, 8179: true, 8180: true, 8182: true, 8183: true, 8305: true, 8319: true, 8336: true, 8337: true, 8338: true, 8339: true, 8340: true, 8341: true, 8342: true, 8343: true, 
	8344: true, 8345: true, 8346: true, 8347: true, 8348: true, 8458: true, 8462: true, 8463: true, 8467: true, 8495: true, 8500: true, 8505: true, 8508: true, 8509: true, 8518: true, 8519: true, 8520: true, 8521: true, 8526: true, 8560: true, 
	8561: true, 8562: true, 8563: true, 8564: true, 8565: true, 8566: true, 8567: true, 8568: true, 8569: true, 8570: true, 8571: true, 8572: true, 8573: true, 8574: true, 8575: true, 8580: true, 9424: true, 9425: true, 9426: true, 9427: true, 
	9428: true, 9429: true, 9430: true, 9431: true, 9432: true, 9433: true, 9434: true, 9435: true, 9436: true, 9437: true, 9438: true, 9439: true, 9440: true, 9441: true, 9442: true, 9443: true, 9444: true, 9445: true, 9446: true, 9447: true, 
	9448: true, 9449: true, 11312: true, 11313: true, 11314: true, 11315: true, 11316: true, 11317: true, 11318: true, 11319: true, 11320: true, 11321: true, 11322: true, 11323: true, 11324: true, 11325: true, 11326: true, 11327: true, 11328: true, 11329: true, 
	11330: true, 11331: true, 11332: true, 11333: true, 11334: true, 11335: true, 11336: true, 11337: true, 11338: true, 11339: true, 11340: true, 11341: true, 11342: true, 11343: true, 11344: true, 11345: true, 11346: true, 11347: true, 11348: true, 11349: true, 
	11350: true, 11351: true, 11352: true, 11353: true, 11354: true, 11355: true, 11356: true, 11357: true, 11358: true, 11361: true, 11365: true, 11366: true, 11368: true, 11370: true, 11372: true, 11377: true, 11379: true, 11380: true, 11382: true, 11383: true, 
	11384: true, 11385: true, 11386: true, 11387: true, 11388: true, 11389: true, 11393: true, 11395: true, 11397: true, 11399: true, 11401: true, 11403: true, 11405: true, 11407: true, 11409: true, 11411: true, 11413: true, 11415: true, 11417: true, 11419: true, 
	11421: true, 11423: true, 11425: true, 11427: true, 11429: true, 11431: true, 11433: true, 11435: true, 11437: true, 11439: true, 11441: true, 11443: true, 11445: true, 11447: true, 11449: true, 11451: true, 11453: true, 11455: true, 11457: true, 11459: true, 
	11461: true, 11463: true, 11465: true, 11467: true, 11469: true, 11471: true, 11473: true, 11475: true, 11477: true, 11479: true, 11481: true, 11483: true, 11485: true, 11487: true, 11489: true, 11491: true, 11492: true, 11500: true, 11502: true, 11507: true, 
	11520: true, 11521: true, 11522: true, 11523: true, 11524: true, 11525: true, 11526: true, 11527: true, 11528: true, 11529: true, 11530: true, 11531: true, 11532: true, 11533: true, 11534: true, 11535: true, 11536: true, 11537: true, 11538: true, 11539: true, 
	11540: true, 11541: true, 11542: true, 11543: true, 11544: true, 11545: true, 11546: true, 11547: true, 11548: true, 11549: true, 11550: true, 11551: true, 11552: true, 11553: true, 11554: true, 11555: true, 11556: true, 11557: true, 11559: true, 11565: true, 
	42561: true, 42563: true, 42565: true, 42567: true, 42569: true, 42571: true, 42573: true, 42575: true, 42577: true, 42579: true, 42581: true, 42583: true, 42585: true, 42587: true, 42589: true, 42591: true, 42593: true, 42595: true, 42597: true, 42599: true, 
	42601: true, 42603: true, 42605: true, 42625: true, 42627: true, 42629: true, 42631: true, 42633: true, 42635: true, 42637: true, 42639: true, 42641: true, 42643: true, 42645: true, 42647: true, 42649: true, 42651: true, 42652: true, 42653: true, 42787: true, 
	42789: true, 42791: true, 42793: true, 42795: true, 42797: true, 42799: true, 42800: true, 42801: true, 42803: true, 42805: true, 42807: true, 42809: true, 42811: true, 42813: true, 42815: true, 42817: true, 42819: true, 42821: true, 42823: true, 42825: true, 
	42827: true, 42829: true, 42831: true, 42833: true, 42835: true, 42837: true, 42839: true, 42841: true, 42843: true, 42845: true, 42847: true, 42849: true, 42851: true, 42853: true, 42855: true, 42857: true, 42859: true, 42861: true, 42863: true, 42864: true, 
	42865: true, 42866: true, 42867: true, 42868: true, 42869: true, 42870: true, 42871: true, 42872: true, 42874: true, 42876: true, 42879: true, 42881: true, 42883: true, 42885: true, 42887: true, 42892: true, 42894: true, 42897: true, 42899: true, 42900: true, 
	42901: true, 42903: true, 42905: true, 42907: true, 42909: true, 42911: true, 42913: true, 42915: true, 42917: true, 42919: true, 42921: true, 42933: true, 42935: true, 43000: true, 43001: true, 43002: true, 43824: true, 43825: true, 43826: true, 43827: true, 
	43828: true, 43829: true, 43830: true, 43831: true, 43832: true, 43833: true, 43834: true, 43835: true, 43836: true, 43837: true, 43838: true, 43839: true, 43840: true, 43841: true, 43842: true, 43843: true, 43844: true, 43845: true, 43846: true, 43847: true, 
	43848: true, 43849: true, 43850: true, 43851: true, 43852: true, 43853: true, 43854: true, 43855: true, 43856: true, 43857: true, 43858: true, 43859: true, 43860: true, 43861: true, 43862: true, 43863: true, 43864: true, 43865: true, 43866: true, 43868: true, 
	43869: true, 43870: true, 43871: true, 43872: true, 43873: true, 43874: true, 43875: true, 43876: true, 43877: true, 43888: true, 43889: true, 43890: true, 43891: true, 43892: true, 43893: true, 43894: true, 43895: true, 43896: true, 43897: true, 43898: true, 
	43899: true, 43900: true, 43901: true, 43902: true, 43903: true, 43904: true, 43905: true, 43906: true, 43907: true, 43908: true, 43909: true, 43910: true, 43911: true, 43912: true, 43913: true, 43914: true, 43915: true, 43916: true, 43917: true, 43918: true, 
	43919: true, 43920: true, 43921: true, 43922: true, 43923: true, 43924: true, 43925: true, 43926: true, 43927: true, 43928: true, 43929: true, 43930: true, 43931: true, 43932: true, 43933: true, 43934: true, 43935: true, 43936: true, 43937: true, 43938: true, 
	43939: true, 43940: true, 43941: true, 43942: true, 43943: true, 43944: true, 43945: true, 43946: true, 43947: true, 43948: true, 43949: true, 43950: true, 43951: true, 43952: true, 43953: true, 43954: true, 43955: true, 43956: true, 43957: true, 43958: true, 
	43959: true, 43960: true, 43961: true, 43962: true, 43963: true, 43964: true, 43965: true, 43966: true, 43967: true, 64256: true, 64257: true, 64258: true, 64259: true, 64260: true, 64261: true, 64262: true, 64275: true, 64276: true, 64277: true, 64278: true, 
	64279: true, 65345: true, 65346: true, 65347: true, 65348: true, 65349: true, 65350: true, 65351: true, 65352: true, 65353: true, 65354: true, 65355: true, 65356: true, 65357: true, 65358: true, 65359: true, 65360: true, 65361: true, 65362: true, 65363: true, 
	65364: true, 65365: true, 65366: true, 65367: true, 65368: true, 65369: true, 65370: true, 66600: true, 66601: true, 66602: true, 66603: true, 66604: true, 66605: true, 66606: true, 66607: true, 66608: true, 66609: true, 66610: true, 66611: true, 66612: true, 
	66613: true, 66614: true, 66615: true, 66616: true, 66617: true, 66618: true, 66619: true, 66620: true, 66621: true, 66622: true, 66623: true, 66624: true, 66625: true, 66626: true, 66627: true, 66628: true, 66629: true, 66630: true, 66631: true, 66632: true, 
	66633: true, 66634: true, 66635: true, 66636: true, 66637: true, 66638: true, 66639: true, 66776: true, 66777: true, 66778: true, 66779: true, 66780: true, 66781: true, 66782: true, 66783: true, 66784: true, 66785: true, 66786: true, 66787: true, 66788: true, 
	66789: true, 66790: true, 66791: true, 66792: true, 66793: true, 66794: true, 66795: true, 66796: true, 66797: true, 66798: true, 66799: true, 66800: true, 66801: true, 66802: true, 66803: true, 66804: true, 66805: true, 66806: true, 66807: true, 66808: true, 
	66809: true, 66810: true, 66811: true, 68800: true, 68801: true, 68802: true, 68803: true, 68804: true, 68805: true, 68806: true, 68807: true, 68808: true, 68809: true, 68810: true, 68811: true, 68812: true, 68813: true, 68814: true, 68815: true, 68816: true, 
	68817: true, 68818: true, 68819: true, 68820: true, 68821: true, 68822: true, 68823: true, 68824: true, 68825: true, 68826: true, 68827: true, 68828: true, 68829: true, 68830: true, 68831: true, 68832: true, 68833: true, 68834: true, 68835: true, 68836: true, 
	68837: true, 68838: true, 68839: true, 68840: true, 68841: true, 68842: true, 68843: true, 68844: true, 68845: true, 68846: true, 68847: true, 68848: true, 68849: true, 68850: true, 71872: true, 71873: true, 71874: true, 71875: true, 71876: true, 71877: true, 
	71878: true, 71879: true, 71880: true, 71881: true, 71882: true, 71883: true, 71884: true, 71885: true, 71886: true, 71887: true, 71888: true, 71889: true, 71890: true, 71891: true, 71892: true, 71893: true, 71894: true, 71895: true, 71896: true, 71897: true, 
	71898: true, 71899: true, 71900: true, 71901: true, 71902: true, 71903: true, 119834: true, 119835: true, 119836: true, 119837: true, 119838: true, 119839: true, 119840: true, 119841: true, 119842: true, 119843: true, 119844: true, 119845: true, 119846: true, 119847: true, 
	119848: true, 119849: true, 119850: true, 119851: true, 119852: true, 119853: true, 119854: true, 119855: true, 119856: true, 119857: true, 119858: true, 119859: true, 119886: true, 119887: true, 119888: true, 119889: true, 119890: true, 119891: true, 119892: true, 119894: true, 
	119895: true, 119896: true, 119897: true, 119898: true, 119899: true, 119900: true, 119901: true, 119902: true, 119903: true, 119904: true, 119905: true, 119906: true, 119907: true, 119908: true, 119909: true, 119910: true, 119911: true, 119938: true, 119939: true, 119940: true, 
	119941: true, 119942: true, 119943: true, 119944: true, 119945: true, 119946: true, 119947: true, 119948: true, 119949: true, 119950: true, 119951: true, 119952: true, 119953: true, 119954: true, 119955: true, 119956: true, 119957: true, 119958: true, 119959: true, 119960: true, 
	119961: true, 119962: true, 119963: true, 119990: true, 119991: true, 119992: true, 119993: true, 119995: true, 119997: true, 119998: true, 119999: true, 120000: true, 120001: true, 120002: true, 120003: true, 120005: true, 120006: true, 120007: true, 120008: true, 120009: true, 
	120010: true, 120011: true, 120012: true, 120013: true, 120014: true, 120015: true, 120042: true, 120043: true, 120044: true, 120045: true, 120046: true, 120047: true, 120048: true, 120049: true, 120050: true, 120051: true, 120052: true, 120053: true, 120054: true, 120055: true, 
	120056: true, 120057: true, 120058: true, 120059: true, 120060: true, 120061: true, 120062: true, 120063: true, 120064: true, 120065: true, 120066: true, 120067: true, 120094: true, 120095: true, 120096: true, 120097: true, 120098: true, 120099: true, 120100: true, 120101: true, 
	120102: true, 120103: true, 120104: true, 120105: true, 120106: true, 120107: true, 120108: true, 120109: true, 120110: true, 120111: true, 120112: true, 120113: true, 120114: true, 120115: true, 120116: true, 120117: true, 120118: true, 120119: true, 120146: true, 120147: true, 
	120148: true, 120149: true, 120150: true, 120151: true, 120152: true, 120153: true, 120154: true, 120155: true, 120156: true, 120157: true, 120158: true, 120159: true, 120160: true, 120161: true, 120162: true, 120163: true, 120164: true, 120165: true, 120166: true, 120167: true, 
	120168: true, 120169: true, 120170: true, 120171: true, 120198: true, 120199: true, 120200: true, 120201: true, 120202: true, 120203: true, 120204: true, 120205: true, 120206: true, 120207: true, 120208: true, 120209: true, 120210: true, 120211: true, 120212: true, 120213: true, 
	120214: true, 120215: true, 120216: true, 120217: true, 120218: true, 120219: true, 120220: true, 120221: true, 120222: true, 120223: true, 120250: true, 120251: true, 120252: true, 120253: true, 120254: true, 120255: true, 120256: true, 120257: true, 120258: true, 120259: true, 
	120260: true, 120261: true, 120262: true, 120263: true, 120264: true, 120265: true, 120266: true, 120267: true, 120268: true, 120269: true, 120270: true, 120271: true, 120272: true, 120273: true, 120274: true, 120275: true, 120302: true, 120303: true, 120304: true, 120305: true, 
	120306: true, 120307: true, 120308: true, 120309: true, 120310: true, 120311: true, 120312: true, 120313: true, 120314: true, 120315: true, 120316: true, 120317: true, 120318: true, 120319: true, 120320: true, 120321: true, 120322: true, 120323: true, 120324: true, 120325: true, 
	120326: true, 120327: true, 120354: true, 120355: true, 120356: true, 120357: true, 120358: true, 120359: true, 120360: true, 120361: true, 120362: true, 120363: true, 120364: true, 120365: true, 120366: true, 120367: true, 120368: true, 120369: true, 120370: true, 120371: true, 
	120372: true, 120373: true, 120374: true, 120375: true, 120376: true, 120377: true, 120378: true, 120379: true, 120406: true, 120407: true, 120408: true, 120409: true, 120410: true, 120411: true, 120412: true, 120413: true, 120414: true, 120415: true, 120416: true, 120417: true, 
	120418: true, 120419: true, 120420: true, 120421: true, 120422: true, 120423: true, 120424: true, 120425: true, 120426: true, 120427: true, 120428: true, 120429: true, 120430: true, 120431: true, 120458: true, 120459: true, 120460: true, 120461: true, 120462: true, 120463: true, 
	120464: true, 120465: true, 120466: true, 120467: true, 120468: true, 120469: true, 120470: true, 120471: true, 120472: true, 120473: true, 120474: true, 120475: true, 120476: true, 120477: true, 120478: true, 120479: true, 120480: true, 120481: true, 120482: true, 120483: true, 
	120484: true, 120485: true, 120514: true, 120515: true, 120516: true, 120517: true, 120518: true, 120519: true, 120520: true, 120521: true, 120522: true, 120523: true, 120524: true, 120525: true, 120526: true, 120527: true, 120528: true, 120529: true, 120530: true, 120531: true, 
	120532: true, 120533: true, 120534: true, 120535: true, 120536: true, 120537: true, 120538: true, 120540: true, 120541: true, 120542: true, 120543: true, 120544: true, 120545: true, 120572: true, 120573: true, 120574: true, 120575: true, 120576: true, 120577: true, 120578: true, 
	120579: true, 120580: true, 120581: true, 120582: true, 120583: true, 120584: true, 120585: true, 120586: true, 120587: true, 120588: true, 120589: true, 120590: true, 120591: true, 120592: true, 120593: true, 120594: true, 120595: true, 120596: true, 120598: true, 120599: true, 
	120600: true, 120601: true, 120602: true, 120603: true, 120630: true, 120631: true, 120632: true, 120633: true, 120634: true, 120635: true, 120636: true, 120637: true, 120638: true, 120639: true, 120640: true, 120641: true, 120642: true, 120643: true, 120644: true, 120645: true, 
	120646: true, 120647: true, 120648: true, 120649: true, 120650: true, 120651: true, 120652: true, 120653: true, 120654: true, 120656: true, 120657: true, 120658: true, 120659: true, 120660: true, 120661: true, 120688: true, 120689: true, 120690: true, 120691: true, 120692: true, 
	120693: true, 120694: true, 120695: true, 120696: true, 120697: true, 120698: true, 120699: true, 120700: true, 120701: true, 120702: true, 120703: true, 120704: true, 120705: true, 120706: true, 120707: true, 120708: true, 120709: true, 120710: true, 120711: true, 120712: true, 
	120714: true, 120715: true, 120716: true, 120717: true, 120718: true, 120719: true, 120746: true, 120747: true, 120748: true, 120749: true, 120750: true, 120751: true, 120752: true, 120753: true, 120754: true, 120755: true, 120756: true, 120757: true, 120758: true, 120759: true, 
	120760: true, 120761: true, 120762: true, 120763: true, 120764: true, 120765: true, 120766: true, 120767: true, 120768: true, 120769: true, 120770: true, 120772: true, 120773: true, 120774: true, 120775: true, 120776: true, 120777: true, 120779: true, 125218: true, 125219: true, 
	125220: true, 125221: true, 125222: true, 125223: true, 125224: true, 125225: true, 125226: true, 125227: true, 125228: true, 125229: true, 125230: true, 125231: true, 125232: true, 125233: true, 125234: true, 125235: true, 125236: true, 125237: true, 125238: true, 125239: true, 
	125240: true, 125241: true, 125242: true, 125243: true, 125244: true, 125245: true, 125246: true, 125247: true, 125248: true, 125249: true, 125250: true, 125251: true
};

function isLower(c){
	var cp = ord(c);
	return isLowerHTable[cp] ? true : false;
}

var isUpperHTable = {
	65: true, 66: true, 67: true, 68: true, 69: true, 70: true, 71: true, 72: true, 73: true, 74: true, 75: true, 76: true, 77: true, 78: true, 79: true, 80: true, 81: true, 82: true, 83: true, 84: true, 
	85: true, 86: true, 87: true, 88: true, 89: true, 90: true, 192: true, 193: true, 194: true, 195: true, 196: true, 197: true, 198: true, 199: true, 200: true, 201: true, 202: true, 203: true, 204: true, 205: true, 
	206: true, 207: true, 208: true, 209: true, 210: true, 211: true, 212: true, 213: true, 214: true, 216: true, 217: true, 218: true, 219: true, 220: true, 221: true, 222: true, 256: true, 258: true, 260: true, 262: true, 
	264: true, 266: true, 268: true, 270: true, 272: true, 274: true, 276: true, 278: true, 280: true, 282: true, 284: true, 286: true, 288: true, 290: true, 292: true, 294: true, 296: true, 298: true, 300: true, 302: true, 
	304: true, 306: true, 308: true, 310: true, 313: true, 315: true, 317: true, 319: true, 321: true, 323: true, 325: true, 327: true, 330: true, 332: true, 334: true, 336: true, 338: true, 340: true, 342: true, 344: true, 
	346: true, 348: true, 350: true, 352: true, 354: true, 356: true, 358: true, 360: true, 362: true, 364: true, 366: true, 368: true, 370: true, 372: true, 374: true, 376: true, 377: true, 379: true, 381: true, 385: true, 
	386: true, 388: true, 390: true, 391: true, 393: true, 394: true, 395: true, 398: true, 399: true, 400: true, 401: true, 403: true, 404: true, 406: true, 407: true, 408: true, 412: true, 413: true, 415: true, 416: true, 
	418: true, 420: true, 422: true, 423: true, 425: true, 428: true, 430: true, 431: true, 433: true, 434: true, 435: true, 437: true, 439: true, 440: true, 444: true, 452: true, 455: true, 458: true, 461: true, 463: true, 
	465: true, 467: true, 469: true, 471: true, 473: true, 475: true, 478: true, 480: true, 482: true, 484: true, 486: true, 488: true, 490: true, 492: true, 494: true, 497: true, 500: true, 502: true, 503: true, 504: true, 
	506: true, 508: true, 510: true, 512: true, 514: true, 516: true, 518: true, 520: true, 522: true, 524: true, 526: true, 528: true, 530: true, 532: true, 534: true, 536: true, 538: true, 540: true, 542: true, 544: true, 
	546: true, 548: true, 550: true, 552: true, 554: true, 556: true, 558: true, 560: true, 562: true, 570: true, 571: true, 573: true, 574: true, 577: true, 579: true, 580: true, 581: true, 582: true, 584: true, 586: true, 
	588: true, 590: true, 880: true, 882: true, 886: true, 895: true, 902: true, 904: true, 905: true, 906: true, 908: true, 910: true, 911: true, 913: true, 914: true, 915: true, 916: true, 917: true, 918: true, 919: true, 
	920: true, 921: true, 922: true, 923: true, 924: true, 925: true, 926: true, 927: true, 928: true, 929: true, 931: true, 932: true, 933: true, 934: true, 935: true, 936: true, 937: true, 938: true, 939: true, 975: true, 
	978: true, 979: true, 980: true, 984: true, 986: true, 988: true, 990: true, 992: true, 994: true, 996: true, 998: true, 1000: true, 1002: true, 1004: true, 1006: true, 1012: true, 1015: true, 1017: true, 1018: true, 1021: true, 
	1022: true, 1023: true, 1024: true, 1025: true, 1026: true, 1027: true, 1028: true, 1029: true, 1030: true, 1031: true, 1032: true, 1033: true, 1034: true, 1035: true, 1036: true, 1037: true, 1038: true, 1039: true, 1040: true, 1041: true, 
	1042: true, 1043: true, 1044: true, 1045: true, 1046: true, 1047: true, 1048: true, 1049: true, 1050: true, 1051: true, 1052: true, 1053: true, 1054: true, 1055: true, 1056: true, 1057: true, 1058: true, 1059: true, 1060: true, 1061: true, 
	1062: true, 1063: true, 1064: true, 1065: true, 1066: true, 1067: true, 1068: true, 1069: true, 1070: true, 1071: true, 1120: true, 1122: true, 1124: true, 1126: true, 1128: true, 1130: true, 1132: true, 1134: true, 1136: true, 1138: true, 
	1140: true, 1142: true, 1144: true, 1146: true, 1148: true, 1150: true, 1152: true, 1162: true, 1164: true, 1166: true, 1168: true, 1170: true, 1172: true, 1174: true, 1176: true, 1178: true, 1180: true, 1182: true, 1184: true, 1186: true, 
	1188: true, 1190: true, 1192: true, 1194: true, 1196: true, 1198: true, 1200: true, 1202: true, 1204: true, 1206: true, 1208: true, 1210: true, 1212: true, 1214: true, 1216: true, 1217: true, 1219: true, 1221: true, 1223: true, 1225: true, 
	1227: true, 1229: true, 1232: true, 1234: true, 1236: true, 1238: true, 1240: true, 1242: true, 1244: true, 1246: true, 1248: true, 1250: true, 1252: true, 1254: true, 1256: true, 1258: true, 1260: true, 1262: true, 1264: true, 1266: true, 
	1268: true, 1270: true, 1272: true, 1274: true, 1276: true, 1278: true, 1280: true, 1282: true, 1284: true, 1286: true, 1288: true, 1290: true, 1292: true, 1294: true, 1296: true, 1298: true, 1300: true, 1302: true, 1304: true, 1306: true, 
	1308: true, 1310: true, 1312: true, 1314: true, 1316: true, 1318: true, 1320: true, 1322: true, 1324: true, 1326: true, 1329: true, 1330: true, 1331: true, 1332: true, 1333: true, 1334: true, 1335: true, 1336: true, 1337: true, 1338: true, 
	1339: true, 1340: true, 1341: true, 1342: true, 1343: true, 1344: true, 1345: true, 1346: true, 1347: true, 1348: true, 1349: true, 1350: true, 1351: true, 1352: true, 1353: true, 1354: true, 1355: true, 1356: true, 1357: true, 1358: true, 
	1359: true, 1360: true, 1361: true, 1362: true, 1363: true, 1364: true, 1365: true, 1366: true, 4256: true, 4257: true, 4258: true, 4259: true, 4260: true, 4261: true, 4262: true, 4263: true, 4264: true, 4265: true, 4266: true, 4267: true, 
	4268: true, 4269: true, 4270: true, 4271: true, 4272: true, 4273: true, 4274: true, 4275: true, 4276: true, 4277: true, 4278: true, 4279: true, 4280: true, 4281: true, 4282: true, 4283: true, 4284: true, 4285: true, 4286: true, 4287: true, 
	4288: true, 4289: true, 4290: true, 4291: true, 4292: true, 4293: true, 4295: true, 4301: true, 5024: true, 5025: true, 5026: true, 5027: true, 5028: true, 5029: true, 5030: true, 5031: true, 5032: true, 5033: true, 5034: true, 5035: true, 
	5036: true, 5037: true, 5038: true, 5039: true, 5040: true, 5041: true, 5042: true, 5043: true, 5044: true, 5045: true, 5046: true, 5047: true, 5048: true, 5049: true, 5050: true, 5051: true, 5052: true, 5053: true, 5054: true, 5055: true, 
	5056: true, 5057: true, 5058: true, 5059: true, 5060: true, 5061: true, 5062: true, 5063: true, 5064: true, 5065: true, 5066: true, 5067: true, 5068: true, 5069: true, 5070: true, 5071: true, 5072: true, 5073: true, 5074: true, 5075: true, 
	5076: true, 5077: true, 5078: true, 5079: true, 5080: true, 5081: true, 5082: true, 5083: true, 5084: true, 5085: true, 5086: true, 5087: true, 5088: true, 5089: true, 5090: true, 5091: true, 5092: true, 5093: true, 5094: true, 5095: true, 
	5096: true, 5097: true, 5098: true, 5099: true, 5100: true, 5101: true, 5102: true, 5103: true, 5104: true, 5105: true, 5106: true, 5107: true, 5108: true, 5109: true, 7680: true, 7682: true, 7684: true, 7686: true, 7688: true, 7690: true, 
	7692: true, 7694: true, 7696: true, 7698: true, 7700: true, 7702: true, 7704: true, 7706: true, 7708: true, 7710: true, 7712: true, 7714: true, 7716: true, 7718: true, 7720: true, 7722: true, 7724: true, 7726: true, 7728: true, 7730: true, 
	7732: true, 7734: true, 7736: true, 7738: true, 7740: true, 7742: true, 7744: true, 7746: true, 7748: true, 7750: true, 7752: true, 7754: true, 7756: true, 7758: true, 7760: true, 7762: true, 7764: true, 7766: true, 7768: true, 7770: true, 
	7772: true, 7774: true, 7776: true, 7778: true, 7780: true, 7782: true, 7784: true, 7786: true, 7788: true, 7790: true, 7792: true, 7794: true, 7796: true, 7798: true, 7800: true, 7802: true, 7804: true, 7806: true, 7808: true, 7810: true, 
	7812: true, 7814: true, 7816: true, 7818: true, 7820: true, 7822: true, 7824: true, 7826: true, 7828: true, 7838: true, 7840: true, 7842: true, 7844: true, 7846: true, 7848: true, 7850: true, 7852: true, 7854: true, 7856: true, 7858: true, 
	7860: true, 7862: true, 7864: true, 7866: true, 7868: true, 7870: true, 7872: true, 7874: true, 7876: true, 7878: true, 7880: true, 7882: true, 7884: true, 7886: true, 7888: true, 7890: true, 7892: true, 7894: true, 7896: true, 7898: true, 
	7900: true, 7902: true, 7904: true, 7906: true, 7908: true, 7910: true, 7912: true, 7914: true, 7916: true, 7918: true, 7920: true, 7922: true, 7924: true, 7926: true, 7928: true, 7930: true, 7932: true, 7934: true, 7944: true, 7945: true, 
	7946: true, 7947: true, 7948: true, 7949: true, 7950: true, 7951: true, 7960: true, 7961: true, 7962: true, 7963: true, 7964: true, 7965: true, 7976: true, 7977: true, 7978: true, 7979: true, 7980: true, 7981: true, 7982: true, 7983: true, 
	7992: true, 7993: true, 7994: true, 7995: true, 7996: true, 7997: true, 7998: true, 7999: true, 8008: true, 8009: true, 8010: true, 8011: true, 8012: true, 8013: true, 8025: true, 8027: true, 8029: true, 8031: true, 8040: true, 8041: true, 
	8042: true, 8043: true, 8044: true, 8045: true, 8046: true, 8047: true, 8120: true, 8121: true, 8122: true, 8123: true, 8136: true, 8137: true, 8138: true, 8139: true, 8152: true, 8153: true, 8154: true, 8155: true, 8168: true, 8169: true, 
	8170: true, 8171: true, 8172: true, 8184: true, 8185: true, 8186: true, 8187: true, 8450: true, 8455: true, 8459: true, 8460: true, 8461: true, 8464: true, 8465: true, 8466: true, 8469: true, 8473: true, 8474: true, 8475: true, 8476: true, 
	8477: true, 8484: true, 8486: true, 8488: true, 8490: true, 8491: true, 8492: true, 8493: true, 8496: true, 8497: true, 8498: true, 8499: true, 8510: true, 8511: true, 8517: true, 8544: true, 8545: true, 8546: true, 8547: true, 8548: true, 
	8549: true, 8550: true, 8551: true, 8552: true, 8553: true, 8554: true, 8555: true, 8556: true, 8557: true, 8558: true, 8559: true, 8579: true, 9398: true, 9399: true, 9400: true, 9401: true, 9402: true, 9403: true, 9404: true, 9405: true, 
	9406: true, 9407: true, 9408: true, 9409: true, 9410: true, 9411: true, 9412: true, 9413: true, 9414: true, 9415: true, 9416: true, 9417: true, 9418: true, 9419: true, 9420: true, 9421: true, 9422: true, 9423: true, 11264: true, 11265: true, 
	11266: true, 11267: true, 11268: true, 11269: true, 11270: true, 11271: true, 11272: true, 11273: true, 11274: true, 11275: true, 11276: true, 11277: true, 11278: true, 11279: true, 11280: true, 11281: true, 11282: true, 11283: true, 11284: true, 11285: true, 
	11286: true, 11287: true, 11288: true, 11289: true, 11290: true, 11291: true, 11292: true, 11293: true, 11294: true, 11295: true, 11296: true, 11297: true, 11298: true, 11299: true, 11300: true, 11301: true, 11302: true, 11303: true, 11304: true, 11305: true, 
	11306: true, 11307: true, 11308: true, 11309: true, 11310: true, 11360: true, 11362: true, 11363: true, 11364: true, 11367: true, 11369: true, 11371: true, 11373: true, 11374: true, 11375: true, 11376: true, 11378: true, 11381: true, 11390: true, 11391: true, 
	11392: true, 11394: true, 11396: true, 11398: true, 11400: true, 11402: true, 11404: true, 11406: true, 11408: true, 11410: true, 11412: true, 11414: true, 11416: true, 11418: true, 11420: true, 11422: true, 11424: true, 11426: true, 11428: true, 11430: true, 
	11432: true, 11434: true, 11436: true, 11438: true, 11440: true, 11442: true, 11444: true, 11446: true, 11448: true, 11450: true, 11452: true, 11454: true, 11456: true, 11458: true, 11460: true, 11462: true, 11464: true, 11466: true, 11468: true, 11470: true, 
	11472: true, 11474: true, 11476: true, 11478: true, 11480: true, 11482: true, 11484: true, 11486: true, 11488: true, 11490: true, 11499: true, 11501: true, 11506: true, 42560: true, 42562: true, 42564: true, 42566: true, 42568: true, 42570: true, 42572: true, 
	42574: true, 42576: true, 42578: true, 42580: true, 42582: true, 42584: true, 42586: true, 42588: true, 42590: true, 42592: true, 42594: true, 42596: true, 42598: true, 42600: true, 42602: true, 42604: true, 42624: true, 42626: true, 42628: true, 42630: true, 
	42632: true, 42634: true, 42636: true, 42638: true, 42640: true, 42642: true, 42644: true, 42646: true, 42648: true, 42650: true, 42786: true, 42788: true, 42790: true, 42792: true, 42794: true, 42796: true, 42798: true, 42802: true, 42804: true, 42806: true, 
	42808: true, 42810: true, 42812: true, 42814: true, 42816: true, 42818: true, 42820: true, 42822: true, 42824: true, 42826: true, 42828: true, 42830: true, 42832: true, 42834: true, 42836: true, 42838: true, 42840: true, 42842: true, 42844: true, 42846: true, 
	42848: true, 42850: true, 42852: true, 42854: true, 42856: true, 42858: true, 42860: true, 42862: true, 42873: true, 42875: true, 42877: true, 42878: true, 42880: true, 42882: true, 42884: true, 42886: true, 42891: true, 42893: true, 42896: true, 42898: true, 
	42902: true, 42904: true, 42906: true, 42908: true, 42910: true, 42912: true, 42914: true, 42916: true, 42918: true, 42920: true, 42922: true, 42923: true, 42924: true, 42925: true, 42926: true, 42928: true, 42929: true, 42930: true, 42931: true, 42932: true, 
	42934: true, 65313: true, 65314: true, 65315: true, 65316: true, 65317: true, 65318: true, 65319: true, 65320: true, 65321: true, 65322: true, 65323: true, 65324: true, 65325: true, 65326: true, 65327: true, 65328: true, 65329: true, 65330: true, 65331: true, 
	65332: true, 65333: true, 65334: true, 65335: true, 65336: true, 65337: true, 65338: true, 66560: true, 66561: true, 66562: true, 66563: true, 66564: true, 66565: true, 66566: true, 66567: true, 66568: true, 66569: true, 66570: true, 66571: true, 66572: true, 
	66573: true, 66574: true, 66575: true, 66576: true, 66577: true, 66578: true, 66579: true, 66580: true, 66581: true, 66582: true, 66583: true, 66584: true, 66585: true, 66586: true, 66587: true, 66588: true, 66589: true, 66590: true, 66591: true, 66592: true, 
	66593: true, 66594: true, 66595: true, 66596: true, 66597: true, 66598: true, 66599: true, 66736: true, 66737: true, 66738: true, 66739: true, 66740: true, 66741: true, 66742: true, 66743: true, 66744: true, 66745: true, 66746: true, 66747: true, 66748: true, 
	66749: true, 66750: true, 66751: true, 66752: true, 66753: true, 66754: true, 66755: true, 66756: true, 66757: true, 66758: true, 66759: true, 66760: true, 66761: true, 66762: true, 66763: true, 66764: true, 66765: true, 66766: true, 66767: true, 66768: true, 
	66769: true, 66770: true, 66771: true, 68736: true, 68737: true, 68738: true, 68739: true, 68740: true, 68741: true, 68742: true, 68743: true, 68744: true, 68745: true, 68746: true, 68747: true, 68748: true, 68749: true, 68750: true, 68751: true, 68752: true, 
	68753: true, 68754: true, 68755: true, 68756: true, 68757: true, 68758: true, 68759: true, 68760: true, 68761: true, 68762: true, 68763: true, 68764: true, 68765: true, 68766: true, 68767: true, 68768: true, 68769: true, 68770: true, 68771: true, 68772: true, 
	68773: true, 68774: true, 68775: true, 68776: true, 68777: true, 68778: true, 68779: true, 68780: true, 68781: true, 68782: true, 68783: true, 68784: true, 68785: true, 68786: true, 71840: true, 71841: true, 71842: true, 71843: true, 71844: true, 71845: true, 
	71846: true, 71847: true, 71848: true, 71849: true, 71850: true, 71851: true, 71852: true, 71853: true, 71854: true, 71855: true, 71856: true, 71857: true, 71858: true, 71859: true, 71860: true, 71861: true, 71862: true, 71863: true, 71864: true, 71865: true, 
	71866: true, 71867: true, 71868: true, 71869: true, 71870: true, 71871: true, 119808: true, 119809: true, 119810: true, 119811: true, 119812: true, 119813: true, 119814: true, 119815: true, 119816: true, 119817: true, 119818: true, 119819: true, 119820: true, 119821: true, 
	119822: true, 119823: true, 119824: true, 119825: true, 119826: true, 119827: true, 119828: true, 119829: true, 119830: true, 119831: true, 119832: true, 119833: true, 119860: true, 119861: true, 119862: true, 119863: true, 119864: true, 119865: true, 119866: true, 119867: true, 
	119868: true, 119869: true, 119870: true, 119871: true, 119872: true, 119873: true, 119874: true, 119875: true, 119876: true, 119877: true, 119878: true, 119879: true, 119880: true, 119881: true, 119882: true, 119883: true, 119884: true, 119885: true, 119912: true, 119913: true, 
	119914: true, 119915: true, 119916: true, 119917: true, 119918: true, 119919: true, 119920: true, 119921: true, 119922: true, 119923: true, 119924: true, 119925: true, 119926: true, 119927: true, 119928: true, 119929: true, 119930: true, 119931: true, 119932: true, 119933: true, 
	119934: true, 119935: true, 119936: true, 119937: true, 119964: true, 119966: true, 119967: true, 119970: true, 119973: true, 119974: true, 119977: true, 119978: true, 119979: true, 119980: true, 119982: true, 119983: true, 119984: true, 119985: true, 119986: true, 119987: true, 
	119988: true, 119989: true, 120016: true, 120017: true, 120018: true, 120019: true, 120020: true, 120021: true, 120022: true, 120023: true, 120024: true, 120025: true, 120026: true, 120027: true, 120028: true, 120029: true, 120030: true, 120031: true, 120032: true, 120033: true, 
	120034: true, 120035: true, 120036: true, 120037: true, 120038: true, 120039: true, 120040: true, 120041: true, 120068: true, 120069: true, 120071: true, 120072: true, 120073: true, 120074: true, 120077: true, 120078: true, 120079: true, 120080: true, 120081: true, 120082: true, 
	120083: true, 120084: true, 120086: true, 120087: true, 120088: true, 120089: true, 120090: true, 120091: true, 120092: true, 120120: true, 120121: true, 120123: true, 120124: true, 120125: true, 120126: true, 120128: true, 120129: true, 120130: true, 120131: true, 120132: true, 
	120134: true, 120138: true, 120139: true, 120140: true, 120141: true, 120142: true, 120143: true, 120144: true, 120172: true, 120173: true, 120174: true, 120175: true, 120176: true, 120177: true, 120178: true, 120179: true, 120180: true, 120181: true, 120182: true, 120183: true, 
	120184: true, 120185: true, 120186: true, 120187: true, 120188: true, 120189: true, 120190: true, 120191: true, 120192: true, 120193: true, 120194: true, 120195: true, 120196: true, 120197: true, 120224: true, 120225: true, 120226: true, 120227: true, 120228: true, 120229: true, 
	120230: true, 120231: true, 120232: true, 120233: true, 120234: true, 120235: true, 120236: true, 120237: true, 120238: true, 120239: true, 120240: true, 120241: true, 120242: true, 120243: true, 120244: true, 120245: true, 120246: true, 120247: true, 120248: true, 120249: true, 
	120276: true, 120277: true, 120278: true, 120279: true, 120280: true, 120281: true, 120282: true, 120283: true, 120284: true, 120285: true, 120286: true, 120287: true, 120288: true, 120289: true, 120290: true, 120291: true, 120292: true, 120293: true, 120294: true, 120295: true, 
	120296: true, 120297: true, 120298: true, 120299: true, 120300: true, 120301: true, 120328: true, 120329: true, 120330: true, 120331: true, 120332: true, 120333: true, 120334: true, 120335: true, 120336: true, 120337: true, 120338: true, 120339: true, 120340: true, 120341: true, 
	120342: true, 120343: true, 120344: true, 120345: true, 120346: true, 120347: true, 120348: true, 120349: true, 120350: true, 120351: true, 120352: true, 120353: true, 120380: true, 120381: true, 120382: true, 120383: true, 120384: true, 120385: true, 120386: true, 120387: true, 
	120388: true, 120389: true, 120390: true, 120391: true, 120392: true, 120393: true, 120394: true, 120395: true, 120396: true, 120397: true, 120398: true, 120399: true, 120400: true, 120401: true, 120402: true, 120403: true, 120404: true, 120405: true, 120432: true, 120433: true, 
	120434: true, 120435: true, 120436: true, 120437: true, 120438: true, 120439: true, 120440: true, 120441: true, 120442: true, 120443: true, 120444: true, 120445: true, 120446: true, 120447: true, 120448: true, 120449: true, 120450: true, 120451: true, 120452: true, 120453: true, 
	120454: true, 120455: true, 120456: true, 120457: true, 120488: true, 120489: true, 120490: true, 120491: true, 120492: true, 120493: true, 120494: true, 120495: true, 120496: true, 120497: true, 120498: true, 120499: true, 120500: true, 120501: true, 120502: true, 120503: true, 
	120504: true, 120505: true, 120506: true, 120507: true, 120508: true, 120509: true, 120510: true, 120511: true, 120512: true, 120546: true, 120547: true, 120548: true, 120549: true, 120550: true, 120551: true, 120552: true, 120553: true, 120554: true, 120555: true, 120556: true, 
	120557: true, 120558: true, 120559: true, 120560: true, 120561: true, 120562: true, 120563: true, 120564: true, 120565: true, 120566: true, 120567: true, 120568: true, 120569: true, 120570: true, 120604: true, 120605: true, 120606: true, 120607: true, 120608: true, 120609: true, 
	120610: true, 120611: true, 120612: true, 120613: true, 120614: true, 120615: true, 120616: true, 120617: true, 120618: true, 120619: true, 120620: true, 120621: true, 120622: true, 120623: true, 120624: true, 120625: true, 120626: true, 120627: true, 120628: true, 120662: true, 
	120663: true, 120664: true, 120665: true, 120666: true, 120667: true, 120668: true, 120669: true, 120670: true, 120671: true, 120672: true, 120673: true, 120674: true, 120675: true, 120676: true, 120677: true, 120678: true, 120679: true, 120680: true, 120681: true, 120682: true, 
	120683: true, 120684: true, 120685: true, 120686: true, 120720: true, 120721: true, 120722: true, 120723: true, 120724: true, 120725: true, 120726: true, 120727: true, 120728: true, 120729: true, 120730: true, 120731: true, 120732: true, 120733: true, 120734: true, 120735: true, 
	120736: true, 120737: true, 120738: true, 120739: true, 120740: true, 120741: true, 120742: true, 120743: true, 120744: true, 120778: true, 125184: true, 125185: true, 125186: true, 125187: true, 125188: true, 125189: true, 125190: true, 125191: true, 125192: true, 125193: true, 
	125194: true, 125195: true, 125196: true, 125197: true, 125198: true, 125199: true, 125200: true, 125201: true, 125202: true, 125203: true, 125204: true, 125205: true, 125206: true, 125207: true, 125208: true, 125209: true, 125210: true, 125211: true, 125212: true, 125213: true, 
	125214: true, 125215: true, 125216: true, 125217: true, 127280: true, 127281: true, 127282: true, 127283: true, 127284: true, 127285: true, 127286: true, 127287: true, 127288: true, 127289: true, 127290: true, 127291: true, 127292: true, 127293: true, 127294: true, 127295: true, 
	127296: true, 127297: true, 127298: true, 127299: true, 127300: true, 127301: true, 127302: true, 127303: true, 127304: true, 127305: true, 127312: true, 127313: true, 127314: true, 127315: true, 127316: true, 127317: true, 127318: true, 127319: true, 127320: true, 127321: true, 
	127322: true, 127323: true, 127324: true, 127325: true, 127326: true, 127327: true, 127328: true, 127329: true, 127330: true, 127331: true, 127332: true, 127333: true, 127334: true, 127335: true, 127336: true, 127337: true, 127344: true, 127345: true, 127346: true, 127347: true, 
	127348: true, 127349: true, 127350: true, 127351: true, 127352: true, 127353: true, 127354: true, 127355: true, 127356: true, 127357: true, 127358: true, 127359: true, 127360: true, 127361: true, 127362: true, 127363: true, 127364: true, 127365: true, 127366: true, 127367: true, 
	127368: true, 127369: true
};

function isUpper(c){
	var cp = ord(c);
	return isUpperHTable[cp] ? true : false;
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
