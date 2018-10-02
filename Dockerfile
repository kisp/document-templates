FROM alpine:edge

RUN echo 'http://dl-cdn.alpinelinux.org/alpine/edge/testing' >>/etc/apk/repositories && \
        apk upgrade --update --no-cache && \
        apk add --no-cache sbcl make

ADD . /src

RUN cd /src && \
        ./configure && \
        make bundle-build && \
        mv document-templates /usr/local/bin

FROM alpine:latest

COPY --from=0 /usr/local/bin/document-templates /usr/local/bin/document-templates

ENTRYPOINT ["document-templates"]
