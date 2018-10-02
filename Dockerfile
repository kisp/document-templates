FROM alpine:edge

RUN echo 'http://dl-cdn.alpinelinux.org/alpine/edge/testing' >>/etc/apk/repositories && \
        apk upgrade --update --no-cache && \
        apk add --no-cache sbcl make

ADD . /src

WORKDIR /src

RUN ./configure && make bundle-build

FROM alpine:latest

COPY --from=0 /src/document-templates /usr/local/bin/document-templates

RUN mkdir /templates

ENV DOCUMENT_TEMPLATES_TEMPLATE_DIRECTORY=/templates

ENTRYPOINT ["document-templates"]
