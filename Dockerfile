FROM alpine:edge

RUN echo 'http://dl-cdn.alpinelinux.org/alpine/edge/testing' >>/etc/apk/repositories && \
        apk upgrade --update --no-cache && \
        apk add --no-cache sbcl make

ADD . /src

WORKDIR /src

RUN ./configure

RUN make bundle-build

RUN mv document-templates /usr/local/bin

WORKDIR /

RUN rm -rf /src

ENTRYPOINT ["document-templates"]
