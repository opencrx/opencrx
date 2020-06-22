/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: CreateSqlTask
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2020, CRIXP Corp., Switzerland
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
package org.opencrx.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.InputFiles
import java.io.File
import org.gradle.kotlin.dsl.*

open class CreateSqlTask : DefaultTask() {

    @get:InputFiles
	var tools = project.getConfigurations().getByName("tools")

	init {
		doLast {
			ant.withGroovyBuilder {
				"taskdef"( 
	  				"name" to "schematool",
	  				"classname" to "org.apache.openjpa.jdbc.ant.SchemaToolTask",
	  				"classpath" to tools.asPath
	  			)
				"schematool"(
					"action" to "add",
					"file" to File(project.getBuildDir(), "generated/sources/sql/" + getProject().property("database.name") + "-add.sql"),
					"primaryKeys" to false,
					"classpath" to tools.asPath
				) {
					"fileset"(
						"dir" to "src/sql"
					) {
						"include"("name" to "openjpa-schema.xml")
					}
					"config"(
						"propertiesFile" to "etc/openjpa/" + project.property("database.name") + "-persistence.xml"
					)
				}
				"schematool"(
					"action" to "build",
					"file" to File(project.getBuildDir(), "generated/sources/sql/" + project.property("database.name") + "-build.sql"),
					"primaryKeys" to false,
					"classpath" to tools.asPath
				) {
					"fileset"(
						"dir" to "src/sql"
					) {
						"include"("name" to "openjpa-schema.xml")
					}
					"config"(
						"propertiesFile" to "etc/openjpa/" + project.property("database.name") + "-persistence.xml"
					)
				}
			}
		}
	}
	
}
