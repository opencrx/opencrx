/**
 * =======================================================================
 * = Description: openCRX/OSSRH build.gradle.kts
 * = Copyright:   (c) 2020 CRIXP AG
 * =======================================================================
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
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */

plugins {
	java
	`java-library`
	eclipse
    `maven-publish`
    signing
}

repositories {
	mavenCentral()
	jcenter()
}

eclipse {
	project {
    	name = "openCRX 4 ~ Publish (jre-" + JavaVersion.current() + ")"
    }
}

publishing {
    repositories {
        maven {
        	// Local
            val releasesRepoUrl = uri("$buildDir/repos/releases")
            val snapshotsRepoUrl = uri("$buildDir/repos/snapshots")
            // OSSRH
            /*
            credentials {
                username = project.property("ossrhUsername").toString()
                password = project.property("ossrhPassword").toString()
            }
            val releasesRepoUrl = uri("https://oss.sonatype.org/service/local/staging/deploy/maven2")
            val snapshotsRepoUrl = uri("https://oss.sonatype.org/content/repositories/snapshots")
            */
            // Url
            url = if (version.toString().endsWith("SNAPSHOT")) snapshotsRepoUrl else releasesRepoUrl
        }
    }
    publications {
        create<MavenPublication>("opencrxGradle") {
            artifactId = "opencrx-gradle"
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/gradle/lib/opencrx-gradle.jar")) { type = "jar" })
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/gradle/lib/opencrx-gradle-sources.jar")) { type = "jar"; classifier = "sources" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-gradle-javadoc.jar")) { type = "jar"; classifier = "javadoc" })
            pom {
                name.set("opencrx-gradle")
                description.set("openCRX/Gradle Library")
                url.set("http://www.opencrx.org")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("wfro")
                        name.set("Werner Froidevaux")
                        email.set("wfro@users.sourceforge.net")
                    }
                    developer {
                        id.set("crixpcmu")
                        name.set("Christoph Mueller")
                        email.set("crixpcmu@users.sourceforge.net")
                    }
                    developer {
                        id.set("dirty-harry")
                        name.set("Harry")
                        email.set("dirty-harry@users.sourceforge.net")
                    }
                    developer {
                        id.set("brunostuder")
                        name.set("Bruno")
                        email.set("brunostuder@users.sourceforge.net")
                    }
                }
                scm {
                    connection.set("scm:git:https://github.com/opencrx/opencrx.git")
                    developerConnection.set("scm:git:ssh://github.com/opencrx/opencrx.git")
                    url.set("https://github.com/opencrx/opencrx/tree/master")
                }
            }
        }
        create<MavenPublication>("opencrxCore") {
            artifactId = "opencrx-core"
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/core/lib/opencrx-core.jar")) { type = "jar" })
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/core/lib/opencrx-core-sources.jar")) { type = "jar"; classifier = "sources" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-core-javadoc.jar")) { type = "jar"; classifier = "javadoc" })
            pom {
                name.set("opencrx-core")
                description.set("openCRX/Core Library")
                url.set("http://www.opencrx.org")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("wfro")
                        name.set("Werner Froidevaux")
                        email.set("wfro@users.sourceforge.net")
                    }
                    developer {
                        id.set("crixpcmu")
                        name.set("Christoph Mueller")
                        email.set("crixpcmu@users.sourceforge.net")
                    }
                    developer {
                        id.set("dirty-harry")
                        name.set("Harry")
                        email.set("dirty-harry@users.sourceforge.net")
                    }
                    developer {
                        id.set("brunostuder")
                        name.set("Bruno")
                        email.set("brunostuder@users.sourceforge.net")
                    }
                }
                scm {
                    connection.set("scm:git:https://github.com/opencrx/opencrx.git")
                    developerConnection.set("scm:git:ssh://github.com/opencrx/opencrx.git")
                    url.set("https://github.com/opencrx/opencrx/tree/master")
                }
            }
        }
        create<MavenPublication>("opencrxCoreModels") {
            artifactId = "opencrx-core-models"
            artifact(project.artifacts.add("archives", File("$rootDir/core/build/generated/sources/model/opencrx-core.openmdx-emf.zip")) { type = "jar" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-core-models-sources.jar")) { type = "jar"; classifier = "sources" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-core-models-javadoc.jar")) { type = "jar"; classifier = "javadoc" })
            pom {
                name.set("opencrx-core-models")
                description.set("openCRX/Core Models Library")
                url.set("http://www.opencrx.org")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("wfro")
                        name.set("Werner Froidevaux")
                        email.set("wfro@users.sourceforge.net")
                    }
                    developer {
                        id.set("crixpcmu")
                        name.set("Christoph Mueller")
                        email.set("crixpcmu@users.sourceforge.net")
                    }
                    developer {
                        id.set("dirty-harry")
                        name.set("Harry")
                        email.set("dirty-harry@users.sourceforge.net")
                    }
                    developer {
                        id.set("brunostuder")
                        name.set("Bruno")
                        email.set("brunostuder@users.sourceforge.net")
                    }
                }
                scm {
                    connection.set("scm:git:https://github.com/opencrx/opencrx.git")
                    developerConnection.set("scm:git:ssh://github.com/opencrx/opencrx.git")
                    url.set("https://github.com/opencrx/opencrx/tree/master")
                }
            }
        }
        create<MavenPublication>("opencrxCoreConfig") {
            artifactId = "opencrx-core-config"
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/core/lib/opencrx-core-config.jar")) { type = "jar" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-core-config-sources.jar")) { type = "jar"; classifier = "sources" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-core-config-javadoc.jar")) { type = "jar"; classifier = "javadoc" })
            pom {
                name.set("opencrx-core-config")
                description.set("openCRX/Core Config Library")
                url.set("http://www.opencrx.org")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("wfro")
                        name.set("Werner Froidevaux")
                        email.set("wfro@users.sourceforge.net")
                    }
                    developer {
                        id.set("crixpcmu")
                        name.set("Christoph Mueller")
                        email.set("crixpcmu@users.sourceforge.net")
                    }
                    developer {
                        id.set("dirty-harry")
                        name.set("Harry")
                        email.set("dirty-harry@users.sourceforge.net")
                    }
                    developer {
                        id.set("brunostuder")
                        name.set("Bruno")
                        email.set("brunostuder@users.sourceforge.net")
                    }
                }
                scm {
                    connection.set("scm:git:https://github.com/opencrx/opencrx.git")
                    developerConnection.set("scm:git:ssh://github.com/opencrx/opencrx.git")
                    url.set("https://github.com/opencrx/opencrx/tree/master")
                }
            }
        }
        create<MavenPublication>("opencrxClient") {
            artifactId = "opencrx-client"
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/core/lib/opencrx-client.jar")) { type = "jar" })
            artifact(project.artifacts.add("archives", File("$rootDir/jre-" + JavaVersion.current() + "/core/lib/opencrx-client-sources.jar")) { type = "jar"; classifier = "sources" })
            artifact(project.artifacts.add("archives", File("src/main/maven/opencrx-client-javadoc.jar")) { type = "jar"; classifier = "javadoc" })
            pom {
                name.set("opencrx-client")
                description.set("openCRX/Client Library")
                url.set("http://www.opencrx.org")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("wfro")
                        name.set("Werner Froidevaux")
                        email.set("wfro@users.sourceforge.net")
                    }
                    developer {
                        id.set("crixpcmu")
                        name.set("Christoph Mueller")
                        email.set("crixpcmu@users.sourceforge.net")
                    }
                    developer {
                        id.set("dirty-harry")
                        name.set("Harry")
                        email.set("dirty-harry@users.sourceforge.net")
                    }
                    developer {
                        id.set("brunostuder")
                        name.set("Bruno")
                        email.set("brunostuder@users.sourceforge.net")
                    }
                }
                scm {
                    connection.set("scm:git:https://github.com/opencrx/opencrx.git")
                    developerConnection.set("scm:git:ssh://github.com/opencrx/opencrx.git")
                    url.set("https://github.com/opencrx/opencrx/tree/master")
                }
            }
        }
    }
}

signing {
    /*
    sign(publishing.publications["opencrxGradle"])
    sign(publishing.publications["opencrxCore"])
    sign(publishing.publications["opencrxCoreModels"])
    sign(publishing.publications["opencrxCoreData"])
    sign(publishing.publications["opencrxClient"])
    */
}
