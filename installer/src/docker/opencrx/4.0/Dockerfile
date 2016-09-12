FROM ubuntu
MAINTAINER docker@opencrx.org
ENV VERSION 4.0.0
ENV PLATFORM jre-1.8
ENV HOME /root
ENV TOMEE_BASEDIR apache-tomee-webprofile-1.7.4

# Install ant, apache2, nullmailer
RUN apt-get update && apt-get -y install \
  openjdk-8-jdk \
  wget \
  unzip \
  ant \
  apache2 \
  libapache2-mod-jk

# Do not expose port 8080 of Tomcat --> front Tomcat with Apache using AJP
RUN echo "<VirtualHost *:80>" > /etc/apache2/sites-available/000-default.conf \
  && echo "  ServerAdmin webmaster@localhost" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  DocumentRoot /var/www/html" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  LogLevel info" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  ErrorLog \${APACHE_LOG_DIR}/error.log" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  CustomLog \${APACHE_LOG_DIR}/access.log combined" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  JkMount /* ajp13_worker" >> /etc/apache2/sites-available/000-default.conf \
  && echo "</VirtualHost>" >> /etc/apache2/sites-available/000-default.conf \
  && apache2ctl restart

# Get and install openCRX
RUN mkdir $HOME/opt \
  && mkdir $HOME/temp \
  && wget -O $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar http://heanet.dl.sourceforge.net/project/opencrx/opencrx/$VERSION/opencrxServer-$VERSION-installer.$PLATFORM.jar \
  && echo "INSTALL_PATH=$HOME/opt/opencrx" > $HOME/temp/opencrx-install.properties \
  && echo "JDKPath=/usr/lib/jvm/java-8-openjdk-amd64" >> $HOME/temp/opencrx-install.properties \
  && echo "ANTPath=/usr/bin/ant" >> $HOME/temp/opencrx-install.properties \
  && java -jar $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar -options $HOME/temp/opencrx-install.properties \
  && ls -al $HOME/opt/opencrx \
  && rm -rf $HOME/temp

VOLUME ["/root/opt/opencrx/data", "/root/opt/opencrx/$TOMEE_BASEDIR/logs", "/root/opt/opencrx/$TOMEE_BASEDIR/temp", "/root/opt/opencrx/$TOMEE_BASEDIR/work", "/root/opt/opencrx/$TOMEE_BASEDIR/airsyncdir", "/root/opt/opencrx/$TOMEE_BASEDIR/maildir"]
COPY ./docker-entrypoint.sh /
RUN chmod a+x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 80 8001 8009
CMD cd /root/opt/opencrx/$TOMEE_BASEDIR/bin;./opencrx.sh run
