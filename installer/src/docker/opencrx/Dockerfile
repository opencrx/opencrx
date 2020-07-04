FROM ubuntu
MAINTAINER docker@opencrx.org
ENV VERSION 4.3.0-rc.1
ENV PLATFORM jre-1.8
ENV HOME /root
ENV TOMEE_BASEDIR apache-tomee-plus-7.0.5

# Install ant, jdk, apache2
RUN apt-get update
RUN apt-get -y install \
  software-properties-common

RUN add-apt-repository ppa:openjdk-r/ppa && apt-get update && apt-get -y install \
  net-tools \
  openjdk-8-jdk \
  wget \
  unzip \
  ant \
  apache2 \
  libapache2-mod-jk

# Do not expose port 8080 of Tomcat --> front Tomcat with Apache using AJP
RUN mkdir -p /etc/apache2/conf
RUN echo "worker.list=ajp13_opencrx" > /etc/apache2/conf/workers.properties \
  && echo "# ajp-opencrx" >> /etc/apache2/conf/workers.properties \
  && echo "worker.ajp13_opencrx.type=ajp13" >> /etc/apache2/conf/workers.properties \
  && echo "worker.ajp13_opencrx.host=localhost" >> /etc/apache2/conf/workers.properties \
  && echo "worker.ajp13_opencrx.port=8009" >> /etc/apache2/conf/workers.properties

RUN echo "<IfModule jk_module>" > /etc/apache2/sites-available/000-default.conf \
  && echo "  JkWorkersFile conf/workers.properties" >> /etc/apache2/sites-available/000-default.conf \
  && echo "</IfModule>" >> /etc/apache2/sites-available/000-default.conf \
  && echo "<VirtualHost *:80>" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  ServerAdmin webmaster@localhost" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  DocumentRoot /var/www/html" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  LogLevel info" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  ErrorLog \${APACHE_LOG_DIR}/error.log" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  CustomLog \${APACHE_LOG_DIR}/access.log combined" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  JkMount /* ajp13_opencrx" >> /etc/apache2/sites-available/000-default.conf \
  && echo "</VirtualHost>" >> /etc/apache2/sites-available/000-default.conf \
  && apache2ctl restart

# Prepare entrypoint
ADD https://raw.githubusercontent.com/opencrx/opencrx/master/installer/src/docker/opencrx/docker-entrypoint.sh /
RUN chmod a+x /docker-entrypoint.sh

# Create and set user opencrx
RUN useradd -m opencrx
ENV HOME /home/opencrx
RUN ls -al $HOME

# Get and install openCRX
RUN mkdir $HOME/opt \
  && mkdir $HOME/temp \
  && wget -O $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar https://github.com/opencrx/opencrx/releases/download/opencrx-v$VERSION/opencrxServer-$VERSION-installer.$PLATFORM.jar \
  && echo "INSTALL_PATH=$HOME/opt/opencrx" > $HOME/temp/opencrx-install.properties \
  && echo "JDKPath=/usr/lib/jvm/java-8-openjdk-amd64" >> $HOME/temp/opencrx-install.properties \
  && echo "ANTPath=/usr/bin/ant" >> $HOME/temp/opencrx-install.properties \
  && cat $HOME/temp/opencrx-install.properties \
  && java -jar $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar -options $HOME/temp/opencrx-install.properties \
  && ls -al $HOME/opt/opencrx \
  && rm -rf $HOME/temp \
  && chown -R opencrx $HOME/opt \
  && chgrp -R opencrx $HOME/opt

ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 80 8001 8009
CMD cd /home/opencrx/opt/opencrx/$TOMEE_BASEDIR/bin;export JDKPath=/usr/lib/jvm/java-8-openjdk-amd64;./opencrx.sh run
