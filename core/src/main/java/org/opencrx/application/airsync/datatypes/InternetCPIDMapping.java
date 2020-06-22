/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.application.airsync.datatypes;

import java.util.HashMap;
import java.util.Map;

public class InternetCPIDMapping {

	private static Map<String, String> mapping;

	static {
		mapping = new HashMap<String, String>();
		mapping.put("ibm037", "37");
		mapping.put("ibm437", "437");
		mapping.put("ibm500", "500");
		mapping.put("asmo-708", "708");
		mapping.put("dos-720", "720");
		mapping.put("ibm737", "737");
		mapping.put("ibm775", "775");
		mapping.put("ibm850", "850");
		mapping.put("ibm852", "852");
		mapping.put("ibm855", "855");
		mapping.put("ibm857", "857");
		mapping.put("ibm00858", "858");
		mapping.put("ibm860", "860");
		mapping.put("ibm861", "861");
		mapping.put("dos-862", "862");
		mapping.put("ibm863", "863");
		mapping.put("ibm864", "864");
		mapping.put("ibm865", "865");
		mapping.put("cp866", "866");
		mapping.put("ibm869", "869");
		mapping.put("ibm870", "870");
		mapping.put("windows-874", "874");
		mapping.put("cp875", "875");
		mapping.put("shift_jis", "932");
		mapping.put("gb2312", "936");
		mapping.put("ks_c_5601-1987", "949");
		mapping.put("big5", "950");
		mapping.put("ibm1026", "1026");
		mapping.put("ibm01047", "1047");
		mapping.put("ibm01140", "1140");
		mapping.put("ibm01141", "1141");
		mapping.put("ibm01142", "1142");
		mapping.put("ibm01143", "1143");
		mapping.put("ibm01144", "1144");
		mapping.put("ibm01145", "1145");
		mapping.put("ibm01146", "1146");
		mapping.put("ibm01147", "1147");
		mapping.put("ibm01148", "1148");
		mapping.put("ibm01149", "1149");
		mapping.put("utf-16", "1200");
		mapping.put("unicodefffe", "1201");
		mapping.put("windows-1250", "1250");
		mapping.put("windows-1251", "1251");
		mapping.put("windows-1252", "1252");
		mapping.put("windows-1253", "1253");
		mapping.put("windows-1254", "1254");
		mapping.put("windows-1255", "1255");
		mapping.put("windows-1256", "1256");
		mapping.put("windows-1257", "1257");
		mapping.put("windows-1258", "1258");
		mapping.put("johab", "1361");
		mapping.put("macintosh", "10000");
		mapping.put("x-mac-japanese", "10001");
		mapping.put("x-mac-chinesetrad", "10002");
		mapping.put("x-mac-korean", "10003");
		mapping.put("x-mac-arabic", "10004");
		mapping.put("x-mac-hebrew", "10005");
		mapping.put("x-mac-greek", "10006");
		mapping.put("x-mac-cyrillic", "10007");
		mapping.put("x-mac-chinesesimp", "10008");
		mapping.put("x-mac-romanian", "10010");
		mapping.put("x-mac-ukrainian", "10017");
		mapping.put("x-mac-thai", "10021");
		mapping.put("x-mac-ce", "10029");
		mapping.put("x-mac-icelandic", "10079");
		mapping.put("x-mac-turkish", "10081");
		mapping.put("x-mac-croatian", "10082");
		mapping.put("x-chinese-cns", "20000");
		mapping.put("x-cp20001", "20001");
		mapping.put("x-chinese-eten", "20002");
		mapping.put("x-cp20003", "20003");
		mapping.put("x-cp20004", "20004");
		mapping.put("x-cp20005", "20005");
		mapping.put("x-ia5", "20105");
		mapping.put("x-ia5-german", "20106");
		mapping.put("x-ia5-swedish", "20107");
		mapping.put("x-ia5-norwegian", "20108");
		mapping.put("us-ascii", "20127");
		mapping.put("x-cp20261", "20261");
		mapping.put("x-cp20269", "20269");
		mapping.put("ibm273", "20273");
		mapping.put("ibm277", "20277");
		mapping.put("ibm278", "20278");
		mapping.put("ibm280", "20280");
		mapping.put("ibm284", "20284");
		mapping.put("ibm285", "20285");
		mapping.put("ibm290", "20290");
		mapping.put("ibm297", "20297");
		mapping.put("ibm420", "20420");
		mapping.put("ibm423", "20423");
		mapping.put("ibm424", "20424");
		mapping.put("x-ebcdic-koreanextended", "20833");
		mapping.put("ibm-thai", "20838");
		mapping.put("koi8-r", "20866");
		mapping.put("ibm871", "20871");
		mapping.put("ibm880", "20880");
		mapping.put("ibm905", "20905");
		mapping.put("ibm00924", "20924");
		mapping.put("euc-jp", "20932");
		mapping.put("x-cp20936", "20936");
		mapping.put("x-cp20949", "20949");
		mapping.put("cp1025", "21025");
		mapping.put("koi8-u", "21866");
		mapping.put("iso-8859-1", "28591");
		mapping.put("iso-8859-2", "28592");
		mapping.put("iso-8859-3", "28593");
		mapping.put("iso-8859-4", "28594");
		mapping.put("iso-8859-5", "28595");
		mapping.put("iso-8859-6", "28596");
		mapping.put("iso-8859-7", "28597");
		mapping.put("iso-8859-8", "28598");
		mapping.put("iso-8859-9", "28599");
		mapping.put("iso-8859-13", "28603");
		mapping.put("iso-8859-15", "28605");
		mapping.put("x-europa", "29001");
		mapping.put("iso-8859-8-i", "38598");
		mapping.put("iso-2022-jp", "50220");
		mapping.put("csiso2022jp", "50221");
		mapping.put("iso-2022-jp", "50222");
		mapping.put("iso-2022-kr", "50225");
		mapping.put("x-cp50227", "50227");
		mapping.put("euc-jp", "51932");
		mapping.put("euc-cn", "51936");
		mapping.put("euc-kr", "51949");
		mapping.put("hz-gb-2312", "52936");
		mapping.put("gb18030", "54936");
		mapping.put("x-iscii-de", "57002");
		mapping.put("x-iscii-be", "57003");
		mapping.put("x-iscii-ta", "57004");
		mapping.put("x-iscii-te", "57005");
		mapping.put("x-iscii-as", "57006");
		mapping.put("x-iscii-or", "57007");
		mapping.put("x-iscii-ka", "57008");
		mapping.put("x-iscii-ma", "57009");
		mapping.put("x-iscii-gu", "57010");
		mapping.put("x-iscii-pa", "57011");
		mapping.put("utf-7", "65000");
		mapping.put("utf-8", "65001");
		mapping.put("utf-32", "65005");
		mapping.put("utf-32be", "65006");
	}

	public static String getInternetCPID(String charset) {
		return mapping.get(charset.toLowerCase());
	}

}
