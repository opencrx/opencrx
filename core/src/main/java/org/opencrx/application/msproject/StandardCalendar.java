/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.openmdx.org/
 * Description: Export activities and resources to MSProject 2003 xml format
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2006, CRIXP Corp., Switzerland
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
 package org.opencrx.application.msproject;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Calendar with working days MO, TU, WE, TH, FR and 8 working hours per day.
 */
class StandardCalendar {
  
    //-------------------------------------------------------------------------
    /**
     * Get working hours between start date and end date.
     */
    public int getWorkingMinutes(
        Date startDate, 
        Date endDate
    ) {
        if (startDate == null || endDate == null) {
          return 0;
        }
        GregorianCalendar start = new GregorianCalendar();
        start.setTime(startDate);
        GregorianCalendar end = new GregorianCalendar();
        end.setTime(endDate);
        
        if (end.before(start)) {
            return 0;
        }
        int workingHours = 0;
        while(start.before(end) || start.equals(end)) {
            if(
               (start.get(Calendar.DAY_OF_WEEK) != Calendar.SATURDAY) &&
               (start.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY)
            ) {
                workingHours += 480;
            }
            start.add(Calendar.DAY_OF_MONTH, 1);
        }
        return workingHours;
  }
  
    //-------------------------------------------------------------------------
    public int getUID(
    ) {
        return 1;
    }
  
    //-------------------------------------------------------------------------
    //saturday, sunday no work day, else 8 hours.
    public String toXml(
    ) {
        return
          "  <Calendar>\n" +
          "    <UID>" + Integer.toString(this.getUID()) + "</UID>\n" +
          "    <Name>Standard</Name>\n" +
          "    <IsBaseCalendar>1</IsBaseCalendar>\n" +
          "    <BaseCalendarUID>-1</BaseCalendarUID>\n" +
          "    <WeekDays>\n" +
          "      <WeekDay>\n" +
          "        <DayType>1</DayType>\n" +
          "        <DayWorking>0</DayWorking>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>2</DayType>\n" +
          "        <DayWorking>1</DayWorking>\n" +
          "        <WorkingTimes>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>08:00:00</FromTime>\n" +
          "            <ToTime>12:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>13:00:00</FromTime>\n" +
          "            <ToTime>17:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "        </WorkingTimes>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>3</DayType>\n" +
          "        <DayWorking>1</DayWorking>\n" +
          "        <WorkingTimes>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>08:00:00</FromTime>\n" +
          "            <ToTime>12:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>13:00:00</FromTime>\n" +
          "            <ToTime>17:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "        </WorkingTimes>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>4</DayType>\n" +
          "        <DayWorking>1</DayWorking>\n" +
          "        <WorkingTimes>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>08:00:00</FromTime>\n" +
          "            <ToTime>12:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>13:00:00</FromTime>\n" +
          "            <ToTime>17:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "        </WorkingTimes>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>5</DayType>\n" +
          "        <DayWorking>1</DayWorking>\n" +
          "        <WorkingTimes>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>08:00:00</FromTime>\n" +
          "            <ToTime>12:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>13:00:00</FromTime>\n" +
          "            <ToTime>17:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "        </WorkingTimes>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>6</DayType>\n" +
          "        <DayWorking>1</DayWorking>\n" +
          "        <WorkingTimes>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>08:00:00</FromTime>\n" +
          "            <ToTime>12:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "          <WorkingTime>\n" +
          "            <FromTime>13:00:00</FromTime>\n" +
          "            <ToTime>17:00:00</ToTime>\n" +
          "          </WorkingTime>\n" +
          "        </WorkingTimes>\n" +
          "      </WeekDay>\n" +
          "      <WeekDay>\n" +
          "        <DayType>7</DayType>\n" +
          "        <DayWorking>0</DayWorking>\n" +
          "      </WeekDay>\n" +
          "    </WeekDays>\n" +
          "  </Calendar>\n";
    }

    //-------------------------------------------------------------------------
    // Variables
    //-------------------------------------------------------------------------
  
}
