FROM liballocs_ubuntu_18

RUN cd /usr/local/src/ && git clone https://github.com/stephenrkell/libcrunch.git

ENV LIBALLOCS /usr/local/src/liballocs

RUN cd /usr/local/src/ && git clone https://github.com/stephenrkell/libdwarf.git
RUN sudo apt-get install -y cmake
RUN cd /usr/local/src/libdwarf && cmake . -B_Release -DCMAKE_BUILD_TYPE=Release
RUN cd /usr/local/src/libdwarf && cmake --build _Release --target dd
RUN cd /usr/local/src/libdwarf && sudo cmake --build _Release --target install

COPY libsystrap_diff /usr/local/src/liballocs/contrib/libsystrap/
RUN cd /usr/local/src/liballocs/contrib/libsystrap && patch -p0 < libsystrap_diff
RUN cd /usr/local/src/liballocs/contrib/libsystrap && make
RUN cd /usr/local/src/liballocs/contrib/libsystrap/contrib/librunt && make

COPY libcrunch_diff /usr/local/src/libcrunch/

RUN cd /usr/local/src/libcrunch && patch -p0 < libcrunch_diff
RUN cd /usr/local/src/libcrunch/src && make -j1
