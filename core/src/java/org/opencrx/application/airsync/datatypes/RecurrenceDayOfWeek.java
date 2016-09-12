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

import java.util.HashSet;
import java.util.Set;

public enum RecurrenceDayOfWeek {
	SU, MO, TU, WE, TH, FR, SA, ;

	public int asInt() {
		switch (this) {
		case SU:
			return 1;
		case MO:
			return 2;
		case TU:
			return 4;
		case WE:
			return 8;
		case TH:
			return 16;
		case FR:
			return 32;
		case SA:
			return 64;
		default:
			return 0;
		}
	}

	public static int dayOfWeekToInt(int i) {
		switch (i) {
		case 1:
			return SU.asInt();
		case 2:
			return MO.asInt();
		case 3:
			return TU.asInt();
		case 4:
			return WE.asInt();
		case 5:
			return TH.asInt();
		case 6:
			return FR.asInt();
		default:
		case 7:
			return SA.asInt();
		}
	}

	public static int asInt(Set<RecurrenceDayOfWeek> dayOfWeek) {
		int pattern = 0;

		if (dayOfWeek.contains(RecurrenceDayOfWeek.MO)) {
			pattern |= RecurrenceDayOfWeek.MO.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.TU)) {
			pattern |= RecurrenceDayOfWeek.TU.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.WE)) {
			pattern |= RecurrenceDayOfWeek.WE.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.TH)) {
			pattern |= RecurrenceDayOfWeek.TH.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.FR)) {
			pattern |= RecurrenceDayOfWeek.FR.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.SA)) {
			pattern |= RecurrenceDayOfWeek.SA.asInt();
		}
		if (dayOfWeek.contains(RecurrenceDayOfWeek.SU)) {
			pattern |= RecurrenceDayOfWeek.SU.asInt();
		}

		return pattern;
	}

	public static Set<RecurrenceDayOfWeek> fromInt(Integer i) {
		HashSet<RecurrenceDayOfWeek> ret = new HashSet<RecurrenceDayOfWeek>();

		if ((i & MO.asInt()) == MO.asInt()) {
			ret.add(MO);
		}
		if ((i & TU.asInt()) == TU.asInt()) {
			ret.add(TU);
		}
		if ((i & WE.asInt()) == WE.asInt()) {
			ret.add(WE);
		}
		if ((i & TH.asInt()) == TH.asInt()) {
			ret.add(TH);
		}
		if ((i & FR.asInt()) == FR.asInt()) {
			ret.add(FR);
		}
		if ((i & SA.asInt()) == SA.asInt()) {
			ret.add(SA);
		}
		if ((i & SU.asInt()) == SU.asInt()) {
			ret.add(SU);
		}
		return ret;
	}

}
