/*
 * ====================================================================
 * Project:     openCRX/Gradle, http://www.opencrx.org/
 * Description: OpencrxPlugin
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

import org.gradle.api.Project
import org.gradle.api.Plugin
import java.io.File
import org.gradle.kotlin.dsl.*

open class OpencrxPluginExtension(project: Project) {
	var earlib = project.getConfigurations().getByName("earlib")
}

open class OpencrxPlugin: Plugin<Project> {

	override fun apply(project: Project): Unit = project.run {
		val extension = extensions.create<OpencrxPluginExtension>("opencrx", project)
		// opencrx-core.jar
		val coreJarTask = tasks.register<CoreJarTask>("opencrx-core.jar")
		coreJarTask {
			dependsOn("classes")
		}
		// opencrx-core-config.jar
		val coreConfigJarTask = tasks.register<CoreConfigJarTask>("opencrx-core-config.jar")
		coreConfigJarTask { dependsOn("classes") }
		// opencrx-client.jar
		val clientJarTask = tasks.register<ClientJarTask>("opencrx-client.jar")
		clientJarTask { dependsOn("classes") }
		// opencrx-config.jar
		tasks.register<ConfigJarTask>("opencrx-config.jar")
		// opencrx-portal.war
		val portalWarTask = tasks.register<PortalWarTask>("opencrx-portal.war")
		portalWarTask { dependsOn("deliverables") }
		// opencrx-ical.war
		val icalWarTask = tasks.register<ICalWarTask>("opencrx-ical.war")
		icalWarTask { dependsOn("deliverables") }
		// opencrx-caldav.war
		val calDavWarTask = tasks.register<CalDavWarTask>("opencrx-caldav.war")
		calDavWarTask { dependsOn("deliverables") }
		// opencrx-carddav.war
		val cardDavWarTask = tasks.register<CardDavWarTask>("opencrx-carddav.war")
		cardDavWarTask { dependsOn("deliverables") }
		// opencrx-webdav.war
		val webDavWarTask = tasks.register<WebDavWarTask>("opencrx-webdav.war")
		webDavWarTask { dependsOn("deliverables") }
		// opencrx-imap.war
		val imapWarTask = tasks.register<ImapWarTask>("opencrx-imap.war")
		imapWarTask { dependsOn("deliverables") }
		// opencrx-vcard.war
		val vCardWarTask = tasks.register<VCardWarTask>("opencrx-vcard.war")
		vCardWarTask { dependsOn("deliverables") }
		// opencrx-spaces.war
		val spacesWarTask = tasks.register<SpacesWarTask>("opencrx-spaces.war")
		spacesWarTask { dependsOn("deliverables") }
		// opencrx-rest.war
		val restWarTask = tasks.register<RestWarTask>("opencrx-rest.war")
		restWarTask { dependsOn("deliverables") }
		// opencrx-ldap.war
		val ldapWarTask = tasks.register<LdapWarTask>("opencrx-ldap.war")
		ldapWarTask { dependsOn("deliverables") }
		// opencrx-bpi.war
		val bpiWarTask = tasks.register<BpiWarTask>("opencrx-bpi.war")
		bpiWarTask { dependsOn("deliverables") }
		// opencrx-calendar.war
		val calendarWarTask = tasks.register<CalendarWarTask>("opencrx-calendar.war")
		calendarWarTask { dependsOn("deliverables") }
		// opencrx-documents.war
		val documentsWarTask = tasks.register<DocumentsWarTask>("opencrx-documents.war")
		documentsWarTask { dependsOn("deliverables") }
		// opencrx-contacts.war
		val contactsWarTask = tasks.register<ContactsWarTask>("opencrx-contacts.war")
		contactsWarTask { dependsOn("deliverables") }
		// opencrx.ear
		val earTask = tasks.register<org.opencrx.gradle.EarTask>("opencrx.ear")
		earTask {
			dependsOn(
				"deliverables",
				"opencrx-config.jar",
				"opencrx-portal.war",
				"opencrx-ical.war",
				"opencrx-caldav.war",
				"opencrx-carddav.war",
				"opencrx-webdav.war",
				"opencrx-imap.war",
				"opencrx-vcard.war",
				"opencrx-spaces.war",
				"opencrx-rest.war",
				"opencrx-ldap.war",
				"opencrx-bpi.war",
				"opencrx-calendar.war",
				"opencrx-documents.war",
				"opencrx-contacts.war"
			)
			libs = extension.earlib
		}
		// render-diagrams
		val renderDiagramsTask = tasks.register<RenderDiagramsTask>("render-diagrams")
		renderDiagramsTask {
			inputs.dir("src/model/graphviz/diagrams")
			classpath = configurations["tools"]
		}
		// create-schema
		tasks.register<org.opencrx.gradle.CreateSchemaTask>("create-schema")
		// create-sql
		tasks.register<CreateSqlTask>("create-sql")
		// generate-model
		val generateModelTask = tasks.register<GenerateModelsTask>("generate-model")
		generateModelTask {
			inputs.dir("${projectDir}/src/model/emf")
			inputs.dir("${projectDir}/src/main/resources")
			outputs.file("${buildDir}/generated/sources/model/opencrx-" + project.getName() + "-models.jar")
			outputs.file("${buildDir}/generated/sources/model/opencrx-" + project.getName() + ".openmdx-xmi.zip")
			classpath = configurations["compileClasspath"]
			doFirst {
				project.copy {
					from(project.zipTree(project.getConfigurations().getByName("openmdxBaseModels").singleFile))
					into(File(project.getBuildDir(), "generated/sources/model/openmdx/base"))
				}
				project.copy {
					from(project.zipTree(project.getConfigurations().getByName("openmdxSecurityModels").singleFile))
					into(File(project.getBuildDir(), "generated/sources/model/openmdx/security"))
				}
				project.copy {
					from(project.zipTree(project.getConfigurations().getByName("openmdxPortalModels").singleFile))
					into(File(project.getBuildDir(), "generated/sources/model/openmdx/portal"))
				}
				if(!project.getConfigurations().getByName("opencrxCoreModels").isEmpty()) {
					project.copy {
						from(project.zipTree(project.getConfigurations().getByName("opencrxCoreModels").singleFile))
						into(File(project.getBuildDir(), "generated/sources/model/opencrx/core"))
					}
				}
			}
			doLast {
				copy {	
					from(
						zipTree("${buildDir}/generated/sources/model/opencrx-" + project.getName() + "-models.zip")
					)
					into("$buildDir/generated/sources/java/main")
					include(
						"**/*.java"
					)
				}
			}
		}
		// configurations
		var implementation = project.getConfigurations().maybeCreate("implementation")
		// opencrx
		project.getConfigurations().maybeCreate("opencrxCoreModels")
		project.getConfigurations().maybeCreate("opencrxCoreConfig")
		// openmdx
		val openmdxVersion = "2.17-20200826"
		project.getConfigurations().maybeCreate("openmdxInspector")
		project.getConfigurations().maybeCreate("openmdxBaseModels")
		project.getConfigurations().maybeCreate("openmdxSecurityModels")
		project.getConfigurations().maybeCreate("openmdxPortalModels")
		project.getConfigurations().maybeCreate("tools")
		// dependencies
		var dependencies = project.getDependencies()
		// implementation
		dependencies.add("implementation", "com.googlecode.libphonenumber:libphonenumber:8.12.0")
		dependencies.add("implementation", "com.google.code.gson:gson:2.7")
		dependencies.add("implementation", "com.google.zxing:core:3.4.0")
		dependencies.add("implementation", "com.itextpdf:itextpdf:5.4.5")
		dependencies.add("implementation", "javax.jdo:jdo-api:3.1")
		dependencies.add("implementation", "javax:javaee-api:8.0")
		dependencies.add("implementation", "org.apache.axis2:axis2:1.5.4")
		dependencies.add("implementation", "org.apache.httpcomponents:httpclient:4.5")
		dependencies.add("implementation", "org.apache.poi:poi:4.1.0")
		dependencies.add("implementation", "org.apache.poi:poi-ooxml:4.1.0")
		dependencies.add("implementation", "org.apache.poi:poi-scratchpad:4.1.0")
		dependencies.add("implementation", "org.apache.pdfbox:pdfbox:2.0.2")
		dependencies.add("implementation", "org.apache.openjpa:openjpa:2.4.3")
		dependencies.add("implementation", "org.apache.xmlbeans:xmlbeans:3.1")
		dependencies.add("implementation", "org.codehaus.janino:janino:3.1.0")
		dependencies.add("implementation", "org.codehaus.janino:commons-compiler:3.1.0")
		dependencies.add("implementation", "org.hsqldb:hsqldb:2.4.0")
		dependencies.add("implementation", "org.igniterealtime.smack:smack:3.2.1")
		dependencies.add("implementation", "org.openmdx:openmdx-base:${openmdxVersion}")
		dependencies.add("implementation", "org.openmdx:openmdx-portal:${openmdxVersion}")
		dependencies.add("implementation", "org.openmdx:openmdx-security:${openmdxVersion}")
		// openmdx
	    dependencies.add("openmdxInspector", "org.openmdx:openmdx-inspector:${openmdxVersion}")
	    dependencies.add("openmdxBaseModels", "org.openmdx:openmdx-base-models:${openmdxVersion}")
	    dependencies.add("openmdxSecurityModels", "org.openmdx:openmdx-security-models:${openmdxVersion}")
	    dependencies.add("openmdxPortalModels", "org.openmdx:openmdx-portal-models:${openmdxVersion}")
		// earlib
		dependencies.add("earlib", "com.google.code.gson:gson:2.7")
		dependencies.add("earlib", "com.google.zxing:core:3.4.0")
		dependencies.add("earlib", "com.googlecode.libphonenumber:libphonenumber:latest.release")
		dependencies.add("earlib", "com.google.protobuf.nano:protobuf-javanano:3.1.0") 	
		dependencies.add("earlib", "org.apache.openjpa:openjpa:2.4.3")
		dependencies.add("earlib", "org.apache.pdfbox:pdfbox:2.0.2")
		dependencies.add("earlib", "org.apache.pdfbox:xmpbox:2.0.2")
		dependencies.add("earlib", "org.apache.pdfbox:preflight:2.0.2")
		dependencies.add("earlib", "org.apache.pdfbox:pdfbox-tools:2.0.2")
		dependencies.add("earlib", "org.apache.poi:poi:4.1.0")
		dependencies.add("earlib", "org.apache.poi:poi-ooxml:4.1.0")
		dependencies.add("earlib", "org.apache.poi:poi-scratchpad:4.1.0")
		dependencies.add("earlib", "org.apache.poi:poi-excelant:4.1.0")
		dependencies.add("earlib", "org.apache.xmlbeans:xmlbeans:3.1")
		dependencies.add("earlib", "org.bouncycastle:bcprov-jdk16:1.46");
		dependencies.add("earlib", "org.codehaus.janino:janino:3.1.0")
		dependencies.add("earlib", "org.codehaus.janino:commons-compiler:3.1.0")
		dependencies.add("earlib", "org.codehaus.groovy:groovy:3.0.1")    
		dependencies.add("earlib", "org.igniterealtime.smack:smack:3.2.1")
		dependencies.add("earlib", "org.igniterealtime.smack:smackx:3.2.1")
		dependencies.add("earlib", "org.igniterealtime.smack:smackx:3.2.1")
		dependencies.add("earlib", "org.igniterealtime.smack:smackx-jingle:3.2.1")
		dependencies.add("earlib", "org.igniterealtime.smack:smackx-debug:3.2.1")
		dependencies.add("earlib", "com.itextpdf:itextpdf:5.4.5")
		dependencies.add("earlib", "org.openmdx:openmdx-base:${openmdxVersion}")
		dependencies.add("earlib", "org.openmdx:openmdx-portal:${openmdxVersion}")
		dependencies.add("earlib", "org.openmdx:openmdx-security:${openmdxVersion}")
		// tools
	    dependencies.add("tools", implementation)
		dependencies.add("tools", "javax:javaee-api:8.0")
		dependencies.add("tools", "org.apache.openjpa:openjpa:2.4.3")
		dependencies.add("tools", "org.hsqldb:hsqldb:2.4.0")
	}
}
