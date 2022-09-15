# First stage: download the Java dependencies (allows them to be cached if unchanged)
#==================================================================================================
FROM maven:3.8.3-adoptopenjdk-11 as retriever

# Copy in Maven settings templates and credentials 
COPY docker/credentials /root/credentials
COPY docker/.m2 /root/.m2

# Populate settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Copy in Java source and build jar
WORKDIR /root/code

COPY pom.xml ./pom.xml
RUN mvn clean dependency:resolve

#==================================================================================================

# Second stage: build war file
#==================================================================================================

FROM maven:3.8.3-adoptopenjdk-11 as builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/
COPY workingdir ./workingdir/
COPY WEB-INF ./WEB-INF/

RUN mvn package -DskipTests -U

#==================================================================================================

# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9 as agent

RUN apt update
RUN apt install build-essential m4 gdal-bin jq -y
WORKDIR /episode

# install specific version of gcc compilers
# ADD https://gfortran.meteodat.ch/download/x86_64/releases/gcc-4.9.4.tar.xz /episode/
# RUN tar -xf gcc-*.tar.xz
# ENV PATH="${PATH}:/episode/gcc-4.9.4/bin/"

# zlib
COPY episode-dependencies/zlib-*.tar.gz /episode/
RUN tar -xvzf zlib-*.tar.gz
RUN mkdir /episode/zlib
RUN (cd zlib-* && ./configure -prefix=/episode/zlib)
RUN (cd zlib-* && make)
RUN (cd zlib-* && make install)

# szip
COPY episode-dependencies/szip-*.tar.gz /episode/
RUN tar -xvzf szip-*.tar.gz
RUN mkdir /episode/szip
RUN (cd szip-* && ./configure -prefix=/episode/szip)
RUN (cd szip-* && make)
RUN (cd szip-* && make install)

# hdf5
COPY episode-dependencies/hdf5-*.tar.gz /episode/
RUN tar -xvzf hdf5-*.tar.gz
RUN mkdir /episode/hdf5
RUN (cd hdf5-* && ./configure --with-zlib=/episode/zlib --prefix=/episode/hdf5)
RUN (cd hdf5-* && make check install)
RUN (cd hdf5-* && make)
RUN (cd hdf5-* && make install)

# netcdf-c
COPY episode-dependencies/netcdf-c-*.tar.gz /episode/
RUN tar -xvzf netcdf-c-*.tar.gz
RUN mkdir /episode/netcdf4
RUN LD_LIBRARY_PATH=/episode/netcdf4/lib
RUN (cd netcdf-c-* && ./configure --prefix=/episode/netcdf4 --enable-shared CPPFLAGS="-I/episode/szip/include -I/episode/hdf5/include -I/episode/netcdf4/include -I/episode/zlib/include" LDFLAGS="-L/episode/szip/lib -L/episode/hdf5/lib -L/episode/netcdf4/lib -L/episode/zlib/lib")
RUN (cd netcdf-c-* && make)
RUN (cd netcdf-c-* && make install)

# netcdf fortran
RUN apt install gfortran -y
COPY episode-dependencies/netcdf-fortran-*.tar.gz /episode/
RUN tar -xvzf netcdf-fortran-*.tar.gz
RUN NCDIR=/episode/netcdf4
RUN CC=/episode/netcdf4/cc
RUN FC=/episode/netcdf4/fortran
RUN export LD_LIBRARY_PATH=${NCDIR}/lib:${LD_LIBRARY_PATH}
RUN NFDIR=/episode/netcdf4
RUN CPPFLAGS=-I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib
RUN (cd netcdf-fortran-* && ./configure --prefix=/episode/netcdf4 --enable-shared CPPFLAGS="-I/episode/szip/include -I/episode/hdf5/include -I/episode/netcdf4/include -I/episode/zlib/include" LDFLAGS="-L/episode/szip/lib -L/episode/hdf5/lib -L/episode/netcdf4/lib -L/episode/zlib/lib")
RUN (cd netcdf-fortran-* && make)
RUN (cd netcdf-fortran-* && make install)

# sqlite
COPY episode-dependencies/sqlite-src-*.zip /episode/
RUN apt install tcl unzip -y
RUN unzip sqlite-src-*.zip
RUN mkdir sqlite3
RUN (cd sqlite-src-* && ./configure --prefix=/episode/sqlite3 --enable-shared SQLITE3_CFLAGS="-I/episode/sqlite3/include" SQLITE3_LIBS="-L/episode/sqlite3/lib")
RUN (cd sqlite-src-* && make)
RUN (cd sqlite-src-* && make install)

# proj
COPY episode-dependencies/proj-*.tar.gz /episode/
RUN apt install sqlite3 pkg-config -y
RUN tar -xvzf proj-*.tar.gz
RUN mkdir proj
RUN (cd proj-* && ./configure --prefix=/episode/proj --enable-shared CPPFLAGS="-I/episode/sqlite3/include" LDFLAGS="-L/episode/sqlite3/lib" SQLITE3_CFLAGS="-I/episode/sqlite3/include" SQLITE3_LIBS="-L/episode/sqlite3/lib -lsqlite3")
RUN (cd proj-* && make)
RUN (cd proj-* && make install)

RUN apt install csh dos2unix gfortran-9 zip -y
COPY citychem-1.3 /episode/citychem-1.3/
RUN (cd citychem-1.3 && find . -type f -print0 | xargs -0 dos2unix)
RUN (cd citychem-1.3 && ./installcc.csh city example)
RUN (cd citychem-1.3 && ./installcc.csh util example)

COPY input.zip /episode/
COPY citychem.sh /episode/
COPY input.json /episode/

WORKDIR /app

# Copy the compiled jar from the builder
COPY --from=builder /root/code/output/* $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

# Run the state updater code
ENTRYPOINT ["./entrypoint.sh"]
